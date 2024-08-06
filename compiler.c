#include <stdio.h>
#include <stdlib.h>

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

typedef void (*ParseFn)();

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

Parser parser;
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

static void endCompiler() {
	emitReturn();
#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(currentChunk(), "code");
	}
#endif
}

static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static void binary() {
	TokenType operatorType = parser.previous.type;
	ParseRule* rule = getRule(operatorType);
	parsePrecedence((Precedence)(rule->precedence + 1));

	switch (operatorType) {
		case TOKEN_PLUS: emitByte(OP_ADD); break;
		case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
		case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
		case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
		default: return;
	}
}

static void grouping() {
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number() {
	double value = strtod(parser.previous.start, NULL);
	emitConstant(value);
}

static void unary() {
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
	[TOKEN_BANG_EQUAL]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_PERCENT]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_PERCENT_EQUAL]     = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AMPER]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AMPER_EQUAL]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_PLUS]              = {NULL,     binary, PREC_TERM},
	[TOKEN_PLUS_EQUAL]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_COLON]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_COLON_EQUAL]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_EQUAL]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_EQUAL_EQUAL]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AT]                = {NULL,     NULL,   PREC_NONE},
	[TOKEN_AT_EQUAL]          = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CIRCUMFLEX]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CIRCUMFLEX_EQUAL]  = {NULL,     NULL,   PREC_NONE},
	[TOKEN_VBAR]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_VBAR_EQUAL]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_MINUS]             = {unary,    binary, PREC_TERM},
	[TOKEN_MINUS_EQUAL]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_GREATER]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_GREATER_EQUAL]     = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LESS]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LESS_EQUAL]        = {NULL,     NULL,   PREC_NONE},
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
	[TOKEN_IDENTIFIER]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_STRING]            = {NULL,     NULL,   PREC_NONE},
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
	[TOKEN_NOT]               = {NULL,     NULL,   PREC_NONE},
	[TOKEN_OR]                = {NULL,     NULL,   PREC_NONE},
	[TOKEN_PASS]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_RAISE]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_RETURN]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_TRY]               = {NULL,     NULL,   PREC_NONE},
	[TOKEN_WHILE]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_WITH]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_YIELD]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FALSE]             = {NULL,     NULL,   PREC_NONE},
	[TOKEN_NONE]              = {NULL,     NULL,   PREC_NONE},
	[TOKEN_TRUE]              = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
	advance();
	ParseFn prefixRule = getRule(parser.previous.type)->prefix;
	if (prefixRule == NULL) {
		error("Expect expression.");
		return;
	}

	prefixRule();

	while (precedence <= getRule(parser.current.type)->precedence) {
		advance();
		ParseFn infixRule = getRule(parser.previous.type)->infix;
		infixRule();
	}
}

static ParseRule* getRule(TokenType type) {
	return &rules[type];
}

static void expression() {
	parsePrecedence(PREC_ASSIGNMENT);
}

bool compile(const char* source, Chunk* chunk) {
	initScanner(source);
	compilingChunk = chunk;

	parser.hadError = false;
	parser.panicMode = false;

	advance();
	expression();
	consume(TOKEN_EOF, "Expect end of expression.");
	endCompiler();
	return !parser.hadError;
}
