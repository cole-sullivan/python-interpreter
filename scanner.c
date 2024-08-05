#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
	const char* start;
	const char* current;
	int line;
	int indentStack[MAX_INDENT_STACK];
	int indentLevel;
	int continuationStack[MAX_CONTINUATION_STACK];
	int continuationLevel;
	bool isAtLineStart;
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
	scanner.start = source;
	scanner.current = source;
	scanner.line = 1;
	scanner.indentLevel = 0;
	scanner.indentStack[0] = 0;
	scanner.continuationLevel = 0;
	scanner.isAtLineStart = true;
}

static bool isAlpha(char c) {
	return (c >= 'a' && c <= 'z') ||
		   (c >= 'A' && c <= 'Z') ||
		   c == '_';
}

static bool isDigit(char c) {
	return c >= '0' && c <= '9';
}

static bool isAtEnd() {
	return *scanner.current == '\0';
}

static Token makeToken(TokenType type) {
	Token token;
	token.type = type;
	token.start = scanner.start;
	token.length = (int)(scanner.current - scanner.start);
	token.line = scanner.line;
	return token;
}

static char advance() {
	scanner.current++;
	return scanner.current[-1];
}

static char peek() {
	return *scanner.current;
}

static char peekNext() {
	if (isAtEnd()) return '\0';
	return scanner.current[1];
}

static bool match(char expected) {
	if (isAtEnd()) return false;
	if (*scanner.current != expected) return false;
	scanner.current++;
	return true;
}

static Token errorToken(const char* message) {
	Token token;
	token.type = TOKEN_ERROR;
	token.start = message;
	token.length = (int)strlen(message);
	token.line = scanner.line;
	return token;
}

static void skipWhitespace() {
	for (;;) {
		char c = peek();
		switch (c) {
			case ' ':
			case '\r':
			case '\t':
				advance();
				break;
			case '\n':
				scanner.line++;
				advance();
				if (scanner.continuationLevel == 0) {
					scanner.isAtLineStart = true;
					return;
				}
				break;
			case '#':
				// A comment goes until the end of the line
				while (peek() != '\n' && !isAtEnd()) advance();
				break;
			default:
				return;
		}
	}
}

static int calculateIndentation() {
	int spaces = 0;
	while (true) {
		char c = peek();
		if (c == ' ') {
			spaces++;
			advance();
		} else if (c == '\t') {
			spaces += 4;
			advance();
		} else break;
	}

	return spaces;
}

static Token parseIndentation() {
	int currentIndent = calculateIndentation();

	if (currentIndent > scanner.indentStack[scanner.indentLevel]) {
		scanner.indentLevel++;
		if (scanner.indentLevel >= MAX_INDENT_STACK)
			return errorToken("Too many levels of indentation.");
		scanner.indentStack[scanner.indentLevel] = currentIndent;
		printf("%d level - %d indentation\n", scanner.indentLevel, scanner.indentStack[scanner.indentLevel]);
		return makeToken(TOKEN_INDENT);
	} else if (currentIndent < scanner.indentStack[scanner.indentLevel]) {
		while (scanner.indentLevel > 0 && currentIndent < scanner.indentStack[scanner.indentLevel])
			scanner.indentLevel--;
		if (currentIndent != scanner.indentStack[scanner.indentLevel])
			return errorToken("Inconsistent indentation.");
		printf("%d level - %d indentation\n", scanner.indentLevel, scanner.indentStack[scanner.indentLevel]);
		return makeToken(TOKEN_DEDENT);
	}

	printf("%d level - %d indentation\n", scanner.indentLevel, scanner.indentStack[scanner.indentLevel]);
	return makeToken(TOKEN_NO_CHANGE);
}

static TokenType checkKeyword(int start, int length,
		const char* rest, TokenType type) {
	if (scanner.current - scanner.start == start + length &&
		memcmp(scanner.start + start, rest, length) == 0) {
		return type;
	}

	return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
	switch (scanner.start[0]) {
		case 'a': 
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'n': return checkKeyword(2, 1, "d", TOKEN_AND);
					case 's':
						if (scanner.current - scanner.start > 2) {
							switch (scanner.start[2]) {
								case 's': return checkKeyword(3, 3, "ert", TOKEN_ASSERT);
								case 'y': return checkKeyword(3, 2, "nc", TOKEN_ASYNC);
							}
						} else return TOKEN_AS;
						break;
					case 'w': return checkKeyword(2, 3, "ait", TOKEN_AWAIT);
				}
			}
			break;
		case 'b': return checkKeyword(1, 4, "reak", TOKEN_BREAK);
		case 'c': 
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'a': return checkKeyword(2, 2, "se", TOKEN_CASE);
					case 'l': return checkKeyword(2, 3, "ass", TOKEN_CLASS);
					case 'o': return checkKeyword(2, 6, "ntinue", TOKEN_CONTINUE);
				}
			}
			break;
		case 'd':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'e':
						if (scanner.current - scanner.start > 2) {
							switch (scanner.start[2]) {
								case 'f': return checkKeyword(0, 3, "def", TOKEN_DEF);
								case 'l': return checkKeyword(0, 3, "del", TOKEN_DEL);
							}
						}
						break;
				}
			}
			break;
		case 'e': 
			if (scanner.current - scanner.start > 1) {
				switch(scanner.start[1]) {
					case 'l':
						if (scanner.current - scanner.start > 2) {
							switch(scanner.start[2]) {
								case 'i': return checkKeyword(3, 1, "f", TOKEN_ELIF);
								case 's': return checkKeyword(3, 3, "e", TOKEN_ELSE);
							}
						}
						break;
					case 'x': return checkKeyword(2, 4, "cept", TOKEN_EXCEPT);
				}
			}
			break;
		case 'f':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'i': return checkKeyword(2, 5, "nally", TOKEN_FINALLY);
					case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
					case 'r': return checkKeyword(2, 2, "om", TOKEN_FROM);
				}
			}
			break;
		case 'g': return checkKeyword(1, 5, "lobal", TOKEN_GLOBAL);
		case 'i':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'f': return TOKEN_IF;
					case 'm': return checkKeyword(2, 4, "port", TOKEN_IMPORT);
					case 'n': return TOKEN_IN;
					case 's': return TOKEN_IS;
				}
			}
			break;
		case 'l': return checkKeyword(1, 5, "ambda", TOKEN_LAMBDA);
		case 'm': return checkKeyword(1, 4, "atch", TOKEN_MATCH);
		case 'n':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'o':
						if (scanner.current - scanner.start > 1) {
							switch (scanner.start[2]) {
								case 'n': return checkKeyword(3, 5, "local", TOKEN_NONLOCAL);
								case 't': return TOKEN_NOT;
							}
						}
						break;
				}
			}
			break;
		case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
		case 'p': return checkKeyword(1, 3, "ass", TOKEN_PASS);
		case 'r': 
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'a': return checkKeyword(2, 3, "ise", TOKEN_RAISE);
					case 'e': return checkKeyword(2, 4, "turn", TOKEN_RETURN);
				}
			}
			break;
		case 't': return checkKeyword(1, 2, "ry", TOKEN_TRY);
		case 'w':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'h': return checkKeyword(2, 3, "ile", TOKEN_WHILE);
					case 'i': return checkKeyword(2, 2, "th", TOKEN_WITH);
				}
			}
			break;
		case 'y': return checkKeyword(1, 4, "ield", TOKEN_YIELD);
		case 'F': return checkKeyword(1, 4, "alse", TOKEN_FALSE);
		case 'N': return checkKeyword(1, 3, "one", TOKEN_NONE);
		case 'T': return checkKeyword(1, 3, "rue", TOKEN_TRUE);
	}

	return TOKEN_IDENTIFIER;
}

static Token identifier() {
	while (isAlpha(peek()) || isDigit(peek())) advance();
	return makeToken(identifierType());
}

static Token number() {
	while (isDigit(peek())) advance();

	// Look for a decimal.
	if (peek() == '.' && isDigit(peekNext())) {
		// Consume the ".".
		advance();
	
		while (isDigit(peek())) advance();
	}

	return makeToken(TOKEN_NUMBER);
}

static Token stringQuotation() {
	while (peek() != '"' && !isAtEnd()) {
		if (peek() == '\n') return errorToken("EOL while scanning string literal.");
		advance();
	}

	if (isAtEnd()) return errorToken("Unterminated string.");

	// The closing quote.
	advance();
	return makeToken(TOKEN_STRING);
}

static Token stringApostrophe() {
	while (peek() != '\'' && !isAtEnd()) {
		if (peek() == '\n') return errorToken("EOL while scanning string literal.");
		advance();
	}

	if (isAtEnd()) return errorToken("Unterminated string.");

	// The closing quote.
	advance();
	return makeToken(TOKEN_STRING);
}

static Token stringMultiline() {
	while (peek() != '"' && !isAtEnd()) {
		if (peek() == '\n') scanner.line++;
		advance();
	}

	if (isAtEnd()) return errorToken("Unterminated string.");

	// The closing quotes.
	advance();
	advance();
	advance();
	return makeToken(TOKEN_STRING);
}

Token scanToken() {
	if (scanner.isAtLineStart) {
		Token indentToken = parseIndentation();
		if (indentToken.type != TOKEN_NO_CHANGE) return indentToken;
		scanner.isAtLineStart = false;
	}

	skipWhitespace();
	scanner.start = scanner.current;

	if (isAtEnd()) return makeToken(TOKEN_EOF);

	char c = advance();

	if (c == '\\' && peek() == '\n') {
		advance();
		scanner.line++;
		return scanToken();
	}

	if (c == '(' || c == '[' || c == '{') {
		if (scanner.continuationLevel >= MAX_CONTINUATION_STACK) {
			return errorToken("Too many nested blocks.");
		}
		scanner.continuationStack[scanner.continuationLevel++] = c;
	} else if (c == ')' || c ==']' || c == '}') {
		if (scanner.continuationLevel <= 0) {
			return errorToken("Unmatched closing bracket.");
		}
		switch (scanner.continuationStack[c]) {
			case '(':
				if (c != ')') return errorToken("Invalid syntax."); 
			case '[':
				if (c != ']') return errorToken("Invalid syntax.");
			case '{':
				if (c != '}') return errorToken("Invalid syntax.");
		}
		scanner.continuationLevel--;
	}

	if (isAlpha(c)) return identifier();
	if (isDigit(c)) return number();
	
	switch (c) {
		case '(': return makeToken(TOKEN_LEFT_PAREN);
		case ')': return makeToken(TOKEN_RIGHT_PAREN);
		case '{': return makeToken(TOKEN_LEFT_BRACE);
		case '}': return makeToken(TOKEN_RIGHT_BRACE);
		case '[': return makeToken(TOKEN_LEFT_BRACKET);
		case ']': return makeToken(TOKEN_RIGHT_BRACKET);
		case ';': return makeToken(TOKEN_SEMICOLON);
		case ',': return makeToken(TOKEN_COMMA);
		case '~': return makeToken(TOKEN_TILDE);
		case '!':
			return makeToken(
				match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
		case '%':
			return makeToken(
				match('=') ? TOKEN_PERCENT_EQUAL : TOKEN_PERCENT);
		case '&':
			return makeToken(
				match('=') ? TOKEN_AMPER_EQUAL : TOKEN_AMPER);
		case '+':
			return makeToken(
				match('=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
		case ':':
			return makeToken(
				match('=') ? TOKEN_COLON_EQUAL : TOKEN_COLON);
		case '=':
			return makeToken(
				match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
		case '@':
			return makeToken(
				match('=') ? TOKEN_AT_EQUAL : TOKEN_AT);
		case '^':
			return makeToken(
				match('^') ? TOKEN_CIRCUMFLEX_EQUAL : TOKEN_CIRCUMFLEX);
		case '|':
			return makeToken(
				match('=') ? TOKEN_VBAR_EQUAL : TOKEN_VBAR);
		case '-':
			if (match('=')) return makeToken(TOKEN_MINUS_EQUAL);
			else if (match('>')) return makeToken(TOKEN_ARROW);
			else return makeToken(TOKEN_MINUS);
		case '<':
			if (match('<')) {
				if (match('=')) return makeToken(TOKEN_LEFT_SHIFT_EQUAL);
				else return makeToken(TOKEN_LEFT_SHIFT);
			} else if (match('=')) return makeToken(TOKEN_LESS_EQUAL);
			else if (match('>')) return makeToken(TOKEN_NOT_EQUAL);
			else return makeToken(TOKEN_LESS);
		case '>':
			if (match('>')) {
				if (match('=')) return makeToken(TOKEN_RIGHT_SHIFT_EQUAL);
				else return makeToken(TOKEN_RIGHT_SHIFT);
			} else if (match('=')) return makeToken(TOKEN_GREATER_EQUAL);
			else return makeToken(TOKEN_GREATER);
		case '*':
			if (match('*')) {
				if (match('=')) return makeToken(TOKEN_DOUBLESTAR_EQUAL);
				else return makeToken(TOKEN_DOUBLESTAR);
			} else if (match('=')) return makeToken(TOKEN_STAR_EQUAL);
			else return makeToken(TOKEN_STAR);
		case '/':
			if (match('/')) {
				if (match('=')) return makeToken(TOKEN_DOUBLESLASH_EQUAL);
				else return makeToken(TOKEN_DOUBLESLASH);
			} else if (match('=')) return makeToken(TOKEN_SLASH_EQUAL);
			else return makeToken(TOKEN_SLASH);
		case '.':
			if (match('.')) {
				if (match('.')) return makeToken(TOKEN_ELLIPSES);
				else return errorToken("Invalid syntax.");
			} else if (isDigit(peek())) return number();
			else return makeToken(TOKEN_DOT);
		case '"': 
			if (match('"')) if (match('"')) return stringMultiline();
			return stringQuotation();
		case '\'': return stringApostrophe();
	}

	return errorToken("Unexpected character.");
}
