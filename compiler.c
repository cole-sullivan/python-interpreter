#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
	Token current;
	Token previous;
	bool hadError;
	bool panicMode;
} Parser;

typedef enum {
	PREC_NONE,
	PREC_ASSIGNMENT, // =, +=, -=, *=, /=
	PREC_LAMBDA,
	PREC_IF,         // if, elif, else
	PREC_OR,         // or
	PREC_AND,        // and
	PREC_NOT,        // not
	PREC_COMPARISON, // is, is not, in, not in, <, <=, >, >=, ==, !=
	PREC_VBAR,       // |
	PREC_CIRCUMFLEX, // ^
	PREC_AMPER,      // &
	PREC_SHIFT,      // <<, >>
	PREC_TERM,       // +, -
	PREC_FACTOR,     // *, /, //, %
	PREC_UNARY,      // +x, -x, ~x
	PREC_DOUBLESTAR, // **
	PREC_CALL,       // ()
	PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

typedef struct {
	Token name;
	int depth;
} Local;

typedef struct {
	Local locals[UINT8_COUNT];
	int localCount;
	int scopeDepth;
} Compiler;

Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;

static Chunk* currentChunk() {
	return compilingChunk;
}

static void errorAt(Token* token, const char* message) {
	if (parser.panicMode) return;
	parser.panicMode = true;
	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF) {
		fprintf(stderr, " at end");
	} else if (token->type == TOKEN_ERROR) {
	} else {
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);
	parser.hadError = true;
}

static void error(const char* message) {
	errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
	errorAt(&parser.current, message);
}

static void advance() {
	parser.previous = parser.current;

	for (;;) {
		parser.current = scanToken();
		if (parser.current.type != TOKEN_ERROR) break;

		errorAtCurrent(parser.current.start);
	}
}

static void consume(TokenType type, const char* message) {
	if (parser.current.type == type) {
		advance();
		return;
	}

	errorAtCurrent(message);
}

static bool check(TokenType type) {
	return parser.current.type == type;
}

static bool match(TokenType type) {
	if (!check(type)) return false;
	advance();
	return true;
}

static void emitByte(uint8_t byte) {
	writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
	emitByte(byte1);
	emitByte(byte2);
}

static void emitReturn() {
	emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
	int constant = addConstant(currentChunk(), value);
	if (constant > UINT8_MAX) {
		error("Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)constant;
}

static void emitConstant(Value value) {
	emitBytes(OP_CONSTANT, makeConstant(value));
}

static void initCompiler(Compiler* compiler) {
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	current = compiler;
}

static void endCompiler() {
	emitReturn();
#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(currentChunk(), "code");
	}
#endif
}

static void beginScope() {
	current->scopeDepth++;
}

static void endScope() {
	current->scopeDepth--;

	while (current->localCount > 0 &&
			current->locals[current->localCount - 1].depth >
				current->scopeDepth) {
		emitByte(OP_POP);
		current->localCount--;
	}
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static uint8_t identifierConstant(Token* name) {
	return makeConstant(OBJ_VAL(copyString(name->start,
											name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
	if (a->length != b->length) return false;
	return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local* local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			return i;
		}
	}

	return -1;
}

static void addLocal(Token name) {
	if (current->localCount == UINT8_COUNT) {
		error("Too many local variables in function.");
		return;
	}

	Local* local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = current->scopeDepth;
}

static void not_(bool canAssign) {
	parsePrecedence(PREC_NOT);
	emitByte(OP_NOT);
}

static void binary(bool canAssign) {
	TokenType operatorType = parser.previous.type;
	ParseRule* rule = getRule(operatorType);
	parsePrecedence((Precedence)(rule->precedence + 1));

	switch (operatorType) {
		case TOKEN_BANG_EQUAL: emitByte(OP_NOT_EQUAL); break;
		case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUAL); break;
		case TOKEN_GREATER: emitByte(OP_GREATER); break;
		case TOKEN_GREATER_EQUAL: emitByte(OP_GREATER_EQUAL); break;
		case TOKEN_LESS: emitByte(OP_LESS); break;
		case TOKEN_LESS_EQUAL: emitByte(OP_LESS_EQUAL); break;
		case TOKEN_PLUS: emitByte(OP_ADD); break;
		case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
		case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
		case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
		default: return;
	}
}

static void literal(bool canAssign) {
	switch (parser.previous.type) {
		case TOKEN_FALSE: emitByte(OP_FALSE); break;
		case TOKEN_NONE: emitByte(OP_NONE); break;
		case TOKEN_TRUE: emitByte(OP_TRUE); break;
		default: return;
	}
}

static void grouping(bool canAssign) {
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
	double value = strtod(parser.previous.start, NULL);
	emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign) {
	emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
									parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
	if (canAssign && check(TOKEN_EQUAL)) {
		if (current->scopeDepth > 0) {
			addLocal(name);
		}
	}

	uint8_t getOp, setOp;
	int arg = resolveLocal(current, &name);

	if (arg != -1) {
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	} else {
		arg = identifierConstant(&name);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
	}

	if (canAssign && match(TOKEN_EQUAL)) {
		expression();
		emitBytes(setOp, (uint8_t)arg);
	} else {
		emitBytes(getOp, (uint8_t)arg);
	}
}

static void variable(bool canAssign) {
	namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
	TokenType operatorType = parser.previous.type;

	parsePrecedence(PREC_UNARY);

	switch(operatorType) {
		case TOKEN_MINUS: emitByte(OP_NEGATE); break;
		default: return;
	}
}

ParseRule rules[] = {
	[TOKEN_LEFT_PAREN]        = {grouping, NULL,   PREC_NONE},
	[TOKEN_RIGHT_PAREN]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LEFT_BRACE]        = {NULL,     NULL,   PREC_NONE}, 
	[TOKEN_RIGHT_BRACE]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LEFT_BRACKET]      = {NULL,     NULL,   PREC_NONE}, 
	[TOKEN_RIGHT_BRACKET]     = {NULL,     NULL,   PREC_NONE},
	[TOKEN_COMMA]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DOT]               = {NULL,     NULL,   PREC_NONE},
	[TOKEN_SEMICOLON]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_TILDE]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_BANG]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_BANG_EQUAL]        = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_PERCENT]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_PERCENT_EQUAL]     = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AMPER]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AMPER_EQUAL]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_PLUS]              = {NULL,     binary, PREC_TERM},
	[TOKEN_PLUS_EQUAL]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_COLON]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_COLON_EQUAL]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_EQUAL]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_EQUAL_EQUAL]       = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_AT]                = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AT_EQUAL]          = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CIRCUMFLEX]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CIRCUMFLEX_EQUAL]  = {NULL,     NULL,   PREC_NONE},
	[TOKEN_VBAR]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_VBAR_EQUAL]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_MINUS]             = {unary,    binary, PREC_TERM},
	[TOKEN_MINUS_EQUAL]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_GREATER]           = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL]     = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_LESS]              = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_LESS_EQUAL]        = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_STAR]              = {NULL,     binary, PREC_FACTOR},
	[TOKEN_STAR_EQUAL]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_SLASH]             = {NULL,     binary, PREC_FACTOR},
	[TOKEN_SLASH_EQUAL]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_NOT_EQUAL]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ARROW]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LEFT_SHIFT]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LEFT_SHIFT_EQUAL]  = {NULL,     NULL,   PREC_NONE},
	[TOKEN_RIGHT_SHIFT]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_RIGHT_SHIFT_EQUAL] = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DOUBLESTAR]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DOUBLESTAR_EQUAL]  = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DOUBLESLASH]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DOUBLESLASH_EQUAL] = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ELLIPSES]          = {NULL,     NULL,   PREC_NONE},
	[TOKEN_IDENTIFIER]        = {variable, NULL,   PREC_NONE},
	[TOKEN_STRING]            = {string,   NULL,   PREC_NONE},
	[TOKEN_NUMBER]            = {number,   NULL,   PREC_NONE},
	[TOKEN_AND]               = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AS]                = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ASSERT]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ASYNC]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AWAIT]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_BREAK]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CASE]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CLASS]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CONTINUE]          = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DEF]               = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DEL]               = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ELIF]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ELSE]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_EXCEPT]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FINALLY]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FOR]               = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FROM]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_GLOBAL]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_IF]                = {NULL,     NULL,   PREC_NONE},
	[TOKEN_IMPORT]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_IN]                = {NULL,     NULL,   PREC_NONE},
	[TOKEN_IS]                = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LAMBDA]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_MATCH]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_NONLOCAL]          = {NULL,     NULL,   PREC_NONE},
	[TOKEN_NOT]               = {not_,     NULL,   PREC_NOT},
	[TOKEN_OR]                = {NULL,     NULL,   PREC_NONE},
	[TOKEN_PASS]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_RAISE]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_RETURN]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_TRY]               = {NULL,     NULL,   PREC_NONE},
	[TOKEN_WHILE]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_WITH]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_YIELD]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FALSE]             = {literal,  NULL,   PREC_NONE},
	[TOKEN_NONE]              = {literal,  NULL,   PREC_NONE},
	[TOKEN_TRUE]              = {literal,  NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
	advance();
	ParseFn prefixRule = getRule(parser.previous.type)->prefix;
	if (prefixRule == NULL) {
		error("Expect expression.");
		return;
	}

	bool canAssign = precedence <= PREC_ASSIGNMENT;
	prefixRule(canAssign);

	while (precedence <= getRule(parser.current.type)->precedence) {
		advance();
		ParseFn infixRule = getRule(parser.previous.type)->infix;
		infixRule(canAssign);
	}

	if (canAssign && match(TOKEN_EQUAL)) {
		error("Invalid assignment target.");
	}
}

static ParseRule* getRule(TokenType type) {
	return &rules[type];
}

static void expression() {
	parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
	while (!check(TOKEN_DEDENT) && !check(TOKEN_EOF)) {
		if (check(TOKEN_NEWLINE)) {
			advance();
			continue;
		}

		declaration();
	}

	if (!check(TOKEN_EOF)) {
		advance();
	}
}

static void expressionStatement() {
	expression();
	emitByte(OP_POP);
}

static void synchronize() {
	parser.panicMode = false;

	while (parser.current.type != TOKEN_EOF) {
		switch (parser.current.type) {
			case TOKEN_CLASS:
			case TOKEN_DEF:
			case TOKEN_FOR:
			case TOKEN_IF:
			case TOKEN_MATCH:
			case TOKEN_WHILE:
			case TOKEN_RETURN:
				return;
			default:
		}

		advance();
	}
}

static void declaration() {
	statement();	

	if (parser.panicMode) synchronize();
}

static void statement() {
	if (match(TOKEN_INDENT)) {
		beginScope();
		block();
		endScope();
	} else {
		expressionStatement();
	}
}

bool compile(const char* source, Chunk* chunk) {
	initScanner(source);
	Compiler compiler;
	initCompiler(&compiler);
	compilingChunk = chunk;

	parser.hadError = false;
	parser.panicMode = false;

	advance();

	while (!match(TOKEN_EOF)) {
		if (check(TOKEN_NEWLINE)) {
			advance();
			continue;
		}

		declaration();
	}

	endCompiler();
	return !parser.hadError;
}
