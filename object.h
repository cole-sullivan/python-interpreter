#ifndef OBJECT_H
#define OBJECT_H

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
#define IS_BUILTIN(value)      isObjType(value, OBJ_BUILTIN)
#define IS_STRING(value)       isObjType(value, OBJ_STRING)

#define AS_FUNCTION(value)     ((ObjFunction*)AS_OBJ(value))
#define AS_BUILTIN(value) \
	(((ObjBuiltin*)AS_OBJ(value))->function)
#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
	OBJ_FUNCTION,
	OBJ_BUILTIN,
	OBJ_STRING,
} ObjType;

struct Obj {
	ObjType type;
	struct Obj* next;
};

typedef struct {
	Obj obj;
	int arity;
	Chunk chunk;
	ObjString* name;
} ObjFunction;

typedef Value (*BuiltinFn)(int argCount, Value* args);

typedef struct {
	Obj obj;
	BuiltinFn function;
} ObjBuiltin;

struct ObjString {
	Obj obj;
	int length;
	char* chars;
	uint32_t hash;
};

ObjFunction* newFunction();
ObjBuiltin* newBuiltin(BuiltinFn function);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
