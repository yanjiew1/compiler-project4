#include "header.h"
#include "symbolTable.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define INTEGER_TMP_REG 9
#define INTEGER_TMP_REG2 10
#define INTEGER_TMP_REG3 11
#define FLOAT_TMP_REG 16
#define FLOAT_TMP_REG2 17

static FILE *outputFile;

/* Register Management */
/*
 * Temperory Register
 *  Integer: x9, x10, x11
 *  Floating Point: s16, s17
 *
 * Frame Pointer: x29
 * Return Address: x30
 */

/* 
 * Activation Record Structure:
 *   Last Frame Pointer
 *   Return Address
 *   Parameters
 *   Local Variables
 *   Other Data
 */

static void genProgram(AST_NODE *root);

/* Function State and Stack Management */
static int stackTop = 0;
static int stackDepth = 0;
static DATA_TYPE returnType = NONE_TYPE;

static void genIntToFloat(int ireg, int freg);
static void genLoadImmInteger64(int reg, long long number);

/* Stack functions */
static void stackPushIntegerReg(int reg);
static void stackPushFloatReg(int reg);
static void stackPushIntegerConst(int number);
static void stackPushFloatConst(float number);
static void stackPushStringConst(char *str);
static void stackPop();
static void stackTopInteger(int reg);
static void stackTopFloat(int reg);

/* Label Management */
static int labelNumber = 0;
static int getLabelNumber();

/* Statements */
static void genStmt(AST_NODE *stmtNode);
static void genBlock(AST_NODE *blockNode);
static void genIfStmt(AST_NODE *ifStmtNode);
static void genWhileStmt(AST_NODE *whileStmtNode);
static void genForStmt(AST_NODE *forStmtNode);
static void genReturnStmt(AST_NODE *returnStmtNode);
static void genAssignmentStmt(AST_NODE *assignmentStmtNode);
static void genFunctionCallStmt(AST_NODE *functionCallStmtNode);

/* Expressions */
static void genExpression(AST_NODE *exprNode);
static void genArthimeticExpression(AST_NODE *exprNode);
static void genAssignmentExpression(AST_NODE *assignNode);
static void genFunctionCallExpression(AST_NODE *functionCallNode);
static void genWriteFuntionCallExpression(AST_NODE *functionCallNode);
static void genReadFuntionCallExpression(AST_NODE *functionCallNode);
static void genIdentifierExpression(AST_NODE *identifierNode);
static void genConstantExpression(AST_NODE *constNode);


/* Global Nodes */
static void genGlobalVariableDecl(AST_NODE* globalVariableNode);
static void genFunctionDecl(AST_NODE* functionNode);

static int getLabelNumber()
{
    return labelNumber++;
}

void codeGen(AST_NODE *root)
{
    outputFile = fopen("output.s", "w");

    if (!outputFile) {
        fprintf(stderr, "Failed to create output.s file.\n");
        exit(1);
    }

    genProgram(root);

    fclose(outputFile);
}

void genProgram(AST_NODE *root)
{
    AST_NODE *node = root->child;

    for (;node;node = node->rightSibling) {
        switch (node->nodeType) {
            case VARIABLE_DECL_LIST_NODE:
                genGlobalVariableDecl(node);
                break;
            case DECLARATION_NODE:
                assert(node->semantic_value.declSemanticValue.kind == FUNCTION_DECL);
                genFunctionDecl(node);
                break;
            default:
                fprintf(stderr, "Unexpected node type at %d\n", node->linenumber);
                exit(1);
            break;
        }
    }
}


static void genIntToFloat(int ireg, int freg)
{
    fprintf(outputFile, "    scvtf s%d, x%d\n", freg, ireg);
}

static void genLoadImmInteger64(int reg, long long number)
{
    int labelNum = getLabelNumber();
    fprintf(outputFile, "    .data\n");
    fprintf(outputFile, "CONSTANT_%d:\n", labelNum);
    fprintf(outputFile, "    .dword %ld\n", number);
    fprintf(outputFile, "    .text\n");
    fprintf(outputFile, "    ldr x%d, CONSTANT_%d\n", reg, labelNum);
}

static void stackPushIntegerReg(int reg)
{
    stackDepth++;
    fprintf(outputFile, "    # Stack Push\n");
    fprintf(outputFile, "    str x%d, [sp, -16]\n", reg);
    fprintf(outputFile, "    sub sp, sp, 16\n");
}

static void stackPushFloatReg(int reg)
{
    stackDepth++;
    fprintf(outputFile, "    # Stack Push\n");
    fprintf(outputFile, "    str s%d, [sp, -16]\n", reg);
    fprintf(outputFile, "    sub sp, sp, 16\n");
}

static void stackPushIntegerConst(int number)
{
    int labelNum = getLabelNumber();
    fprintf(outputFile, "    # Load Const Into Stack\n");
    fprintf(outputFile, "    .data\n");
    fprintf(outputFile, "CONSTANT_%d:\n", labelNum);
    fprintf(outputFile, "    .word %d\n", number);
    fprintf(outputFile, "    .align 3\n");
    fprintf(outputFile, "    .text\n");
    fprintf(outputFile, "    ldr w%d, CONSTANT_%d\n", INTEGER_TMP_REG, labelNum);
    stackPushIntegerReg(INTEGER_TMP_REG);
}

static void stackPushFloatConst(float number)
{
    int labelNum = getLabelNumber();
    fprintf(outputFile, "    # Load Const Into Stack\n");
    fprintf(outputFile, "    .data\n");
    fprintf(outputFile, "CONSTANT_%d:\n", labelNum);
    fprintf(outputFile, "    .float %f\n", number);
    fprintf(outputFile, "    .align 3\n");
    fprintf(outputFile, "    .text\n");
    fprintf(outputFile, "    ldr s%d, CONSTANT_%d\n", FLOAT_TMP_REG, labelNum);
    stackPushFloatReg(FLOAT_TMP_REG);
}

static void stackPushStringConst(char *str)
{
    int labelNum = getLabelNumber();
    fprintf(outputFile, "    # Load Const Into Stack\n");
    fprintf(outputFile, "    .data\n");
    fprintf(outputFile, "CONSTANT_%d:\n", labelNum);
    fprintf(outputFile, "    .asciz %s\n", str);
    fprintf(outputFile, "    .align 3\n");
    fprintf(outputFile, "    .text\n");
    fprintf(outputFile, "    ldr x%d, =CONSTANT_%d\n", INTEGER_TMP_REG, labelNum);
    stackPushIntegerReg(INTEGER_TMP_REG);
}

static void stackPop()
{
    stackDepth--;
    fprintf(outputFile, "    # Stack Pop\n");
    fprintf(outputFile, "    add sp, sp, 16\n");
}


static void stackTopInteger(int reg)
{
    fprintf(outputFile, "    # Stack Top\n");
    fprintf(outputFile, "    ldr w%d, [sp]\n", reg);
}

static void stackTopFloat(int reg)
{
    fprintf(outputFile, "    # Stack Top\n");
    fprintf(outputFile, "    ldr s%d, [sp]\n", reg);
}

void genGlobalVariableDecl(AST_NODE* globalVariableNode)
{

    AST_NODE* declNode = globalVariableNode->child;

    fprintf(outputFile, "# Global Variables (line: %d)\n", globalVariableNode->linenumber);
    fprintf(outputFile, "    .data\n");

    for (;declNode;declNode = declNode->rightSibling)
    {
        AST_NODE *idNode = declNode->child;
        for (;idNode;idNode = idNode->rightSibling) {
            SymbolTableEntry *symbol = idNode->semantic_value.identifierSemanticValue.symbolTableEntry;
            /* 只考慮 variable */
            if (symbol->attribute->attributeKind != VARIABLE_ATTRIBUTE) {
                continue;
            }

            fprintf(outputFile, "# Variable %s (line: %d)\n", symbol->name, idNode->linenumber);
            TypeDescriptor *typeDescriptor = symbol->attribute->attr.typeDescriptor;
            TypeDescriptorKind typeKind = typeDescriptor->kind;
            fprintf(outputFile, "%s:\n", symbol->name);

            if (typeKind == SCALAR_TYPE_DESCRIPTOR) {
                /* 純量 */
                if (idNode->semantic_value.identifierSemanticValue.kind == WITH_INIT_ID) {
                    AST_NODE *exprNode = idNode->child;
                    switch (typeDescriptor->properties.dataType) {
                        case INT_TYPE:
                            fprintf(outputFile, "    .word   %d\n",
                                exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue);
                        break;
                        case FLOAT_TYPE:
                            fprintf(outputFile, "    .float  %f\n",
                                exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue);
                        break;
                        default:
                            fprintf(stderr, "TypeError: Unsupported type at %d\n", idNode->linenumber);
                            exit(1);
                    }
                } else {
                    switch (typeDescriptor->properties.dataType) {
                        case INT_TYPE:
                            fprintf(outputFile, "    .word   0\n");
                        break;
                        case FLOAT_TYPE:
                            fprintf(outputFile, "    .float  0.0\n");
                        break;
                        default:
                            fprintf(stderr, "TypeError: Unsupported type at %d\n", idNode->linenumber);
                            exit(1);
                    }
                }
                fprintf(outputFile, "    .align 3\n");
            } else {
                /* 陣列 */
                int nElements = 1;
                int size;

                for (int i = 0; i < typeDescriptor->properties.arrayProperties.dimension; i++) {
                    nElements *= typeDescriptor->properties.arrayProperties.sizeInEachDimension[i];
                }

                size = nElements << 2;
                fprintf(outputFile, "%s:\n", symbol->name);
                fprintf(outputFile, "    .space %d\n", size);
                fprintf(outputFile, "    .align 3\n");
            }
        }
    }

    fprintf(outputFile, "\n");
}

void genFunctionDecl(AST_NODE* functionNode)
{
    AST_NODE *idNode = functionNode->child->rightSibling;
    AST_NODE *blockNode = idNode->rightSibling->rightSibling;
    SymbolTableEntry *symbol = idNode->semantic_value.identifierSemanticValue.symbolTableEntry;
    FunctionSignature *functionSignature = symbol->attribute->attr.functionSignature;

    /* Function 前置程式*/
    fprintf(outputFile, "# Function %s (line: %d)\n", symbol->name, functionNode->linenumber);
    fprintf(outputFile, "    .text\n");
    fprintf(outputFile, "%s:\n", symbol->name);

    if (strcmp(symbol->name, "main") == 0 || strcmp(symbol->name, "MAIN") == 0) {
        fprintf(outputFile, "_start_MAIN:\n");
    }

    /* 把 fp 和 ra 放入 stack ，並修改 fp */
    fprintf(outputFile, "    stp x29, x30, [sp, -16]\n");
    fprintf(outputFile, "    mov x29, sp\n");
    fprintf(outputFile, "    sub sp, sp, 16\n");
    stackTop = -16;

    /* TODO: 把參數放入 Stack ，此次作業不處理 */

    /* 處理 block */
    genBlock(blockNode);

    /* Function 後置程式: 回復 sp ，讀回 fp 、 ra 和 x19 */
    fprintf(outputFile, "# Function Return\n");
    fprintf(outputFile, "    mov sp, x29\n");
    fprintf(outputFile, "    ldp x29, x30, [sp, -16]\n");
    fprintf(outputFile, "    ret\n");
    fprintf(outputFile, "# End of function %s\n", symbol->name);
}


void genStmt(AST_NODE *stmtNode)
{
    fprintf(outputFile, "# Statement at line %d\n", stmtNode->linenumber);

    if (stmtNode->nodeType == BLOCK_NODE) {
        genBlock(stmtNode);
    } else if (stmtNode->nodeType == NUL_NODE) {
    } else {
        switch (stmtNode->semantic_value.stmtSemanticValue.kind) {
            case IF_STMT: genIfStmt(stmtNode); break;
            case WHILE_STMT: genWhileStmt(stmtNode); break;
            case ASSIGN_STMT: genAssignmentStmt(stmtNode); break;
            case FUNCTION_CALL_STMT: genFunctionCallStmt(stmtNode); break;
            case RETURN_STMT: genReturnStmt(stmtNode); break;
            default:
                fprintf(stderr, "Unknown statement type at %d\n", stmtNode->linenumber);
                exit(1);
        }
    }
}

void genBlock(AST_NODE *blockNode)
{
    fprintf(outputFile, "# Beginning of Block\n", blockNode->linenumber);

    AST_NODE *child = blockNode->child;

    /* 處理變數宣告 */
    int blockStackSize = 0;
    if (child && child->nodeType == VARIABLE_DECL_LIST_NODE) {
        fprintf(outputFile, "# Variable Declarations (line: %d)\n", child->linenumber);
        AST_NODE *declNode = child->child;
        for (;declNode;declNode = declNode->rightSibling) {
            AST_NODE *idNode = declNode->child;

            for (;idNode;idNode = idNode->rightSibling) {
                int size = 0;
                SymbolTableEntry *symbol = idNode->semantic_value.identifierSemanticValue.symbolTableEntry;

                /* 只考慮 variable */
                if (symbol->attribute->attributeKind != VARIABLE_ATTRIBUTE) {
                    continue;
                }

                TypeDescriptor *typeDescriptor = symbol->attribute->attr.typeDescriptor;
                TypeDescriptorKind typeKind = typeDescriptor->kind;

                if (typeKind == SCALAR_TYPE_DESCRIPTOR) {
                    /* 純量 */
                    size = 16;
                } else {
                    /* 陣列 */
                    int nElements = 1;

                    for (int i = 0; i < typeDescriptor->properties.arrayProperties.dimension; i++) {
                        nElements *= typeDescriptor->properties.arrayProperties.sizeInEachDimension[i];
                    }

                    /* stack 要 16 bytes 對齊 */
                    size = nElements << 2;
                    if (size % 16 != 0) {
                        size += 16 - (size % 16);
                    }
                }
                stackTop -= size;
                blockStackSize += size;

                symbol->offset = stackTop;
                fprintf(outputFile, "# Variable %s (line: %d) Frame Offset: %d\n", symbol->name, idNode->linenumber, stackTop);
            }
        }
        fprintf(outputFile, "# End of Variable Declarations\n");
        fprintf(outputFile, "\n");
        fprintf(outputFile, "    sub sp, sp, %d\n", blockStackSize);
        fprintf(outputFile, "# Variable Initializations\n");
        declNode = child->child;
        for (;declNode;declNode = declNode->rightSibling) {
            AST_NODE *idNode = declNode->child;
            for (;idNode;idNode = idNode->rightSibling) {
                SymbolTableEntry *symbol = idNode->semantic_value.identifierSemanticValue.symbolTableEntry;

                /* 只考慮 variable */
                if (symbol->attribute->attributeKind != VARIABLE_ATTRIBUTE) {
                    continue;
                }

                TypeDescriptor *typeDescriptor = symbol->attribute->attr.typeDescriptor;
                TypeDescriptorKind typeKind = typeDescriptor->kind;

                /* 只考慮純量 */
                if (typeKind != SCALAR_TYPE_DESCRIPTOR) {
                    continue;
                }

                /* 沒有初始值 */
                if (idNode->semantic_value.identifierSemanticValue.kind != WITH_INIT_ID) {
                    continue;
                }

                fprintf(outputFile, "# Initialize variable %s\n", symbol->name);
                genExpression(idNode->child);

                switch (typeDescriptor->properties.dataType) {
                    case INT_TYPE:
                        stackTopInteger(INTEGER_TMP_REG);
                        fprintf(outputFile, "    str w%d, [x29, %d]\n", INTEGER_TMP_REG, symbol->offset);
                        break;
                    case FLOAT_TYPE:
                        stackTopFloat(FLOAT_TMP_REG);
                        fprintf(outputFile, "    str s%d, [x29, %d]\n", FLOAT_TMP_REG, symbol->offset);
                        break;
                    default:
                        fprintf(stderr, "TypeError: Unsupported type for variable initialization at %d\n", idNode->linenumber);
                        exit(1);
                }

                stackPop();
            }
        }

        fprintf(outputFile, "# End of Variable Initializations\n");
        fprintf(outputFile, "\n");
        child = child->rightSibling;
    }

    if (child && child->nodeType == STMT_LIST_NODE) {
        fprintf(outputFile, "# Start of Statement List\n", child->linenumber);
        AST_NODE *stmtNode = child->child;

        for (;stmtNode;stmtNode = stmtNode->rightSibling) {
            genStmt(stmtNode);
        }
        fprintf(outputFile, "# End of Statement List\n");
        fprintf(outputFile, "\n");
    }

    /* 回復 stack top */
    stackTop += blockStackSize;
    fprintf(outputFile, "    add sp, sp, %d\n", blockStackSize);
}

static void genIfStmt(AST_NODE *ifStmtNode)
{
    int labelNum = getLabelNumber();
    AST_NODE *testExpression = ifStmtNode->child;
    AST_NODE *ifBody = testExpression->rightSibling;
    AST_NODE *elseBody = ifBody->rightSibling;

    fprintf(outputFile, "# If Statement\n", ifStmtNode->linenumber);

    if (testExpression->semantic_value.exprSemanticValue.isConstEval) {
        /* Dead code elimination */
        int evalResult = 0;

        if (testExpression->dataType == INT_TYPE) {
            evalResult = testExpression->semantic_value.exprSemanticValue.constEvalValue.iValue;
        } else {
            evalResult = testExpression->semantic_value.exprSemanticValue.constEvalValue.fValue;
        }

        if (evalResult) {
            genStmt(ifBody);
        } else {
            genStmt(elseBody);
        }

    } else {
        /* 條件判斷 */
        genExpression(testExpression);
        stackTopInteger(INTEGER_TMP_REG);
        stackPop();
        fprintf(outputFile, "    cmp w%d, 0\n", INTEGER_TMP_REG);
        fprintf(outputFile, "    beq ELSE_%d\n", labelNum);

        /* If Body */
        fprintf(outputFile, "# If Body (line %d)\n", ifBody->linenumber);
        genStmt(ifBody);
        fprintf(outputFile, "    b END_IF_%d\n", labelNum);

        /* Else Body */
        fprintf(outputFile, "# Else Body (line %d)\n", elseBody->linenumber);
        fprintf(outputFile, "ELSE_%d:\n", labelNum);
        genStmt(elseBody);

        /* End If */
        fprintf(outputFile, "END_IF_%d:\n", labelNum);
        fprintf(outputFile, "# End If Statement\n", ifStmtNode->linenumber);
        fprintf(outputFile, "\n");
    }
}

static void genWhileStmt(AST_NODE *whileStmtNode)
{
    int labelNum = getLabelNumber();
    AST_NODE *testExpression = whileStmtNode->child;
    AST_NODE *body = testExpression->rightSibling;

    fprintf(outputFile, "# While Statement\n", whileStmtNode->linenumber);

    /* 產生 while label */
    fprintf(outputFile, "WHILE_%d:\n", labelNum);

    /* 條件判斷 */
    genExpression(testExpression);
    stackTopInteger(INTEGER_TMP_REG);
    stackPop();
    fprintf(outputFile, "    cmp w%d, 0\n", INTEGER_TMP_REG);
    fprintf(outputFile, "    beq END_WHILE_%d\n", labelNum);

    fprintf(outputFile, "# While Body (line: %d)\n", body->linenumber);
    /* Body */
    genStmt(body);

    /* 跳回前面迴圈判斷 */
    fprintf(outputFile, "    b WHILE_%d\n", labelNum);

    /* End While */
    fprintf(outputFile, "END_WHILE_%d:\n", labelNum);
    fprintf(outputFile, "# End While Statement\n", whileStmtNode->linenumber);
    fprintf(outputFile, "\n");

}

static void genForStmt(AST_NODE *forStmtNode)
{
    /* TODO */
}

static void genReturnStmt(AST_NODE *returnStmtNode)
{
    fprintf(outputFile, "# Return Statement\n", returnStmtNode->linenumber);
    AST_NODE *exprNode = returnStmtNode->child;

    if (exprNode->nodeType != NUL_NODE) {
        genExpression(exprNode);
        switch (returnStmtNode->dataType)
        {
            case INT_TYPE:
                stackTopInteger(INTEGER_TMP_REG);
                fprintf(outputFile, "    mov x0, x%d\n", INTEGER_TMP_REG);
                break;
            case FLOAT_TYPE:
                stackTopFloat(FLOAT_TMP_REG);
                fprintf(outputFile, "    fmov s0, s%d\n", FLOAT_TMP_REG);
                break;
        }
        stackPop();
    }

    fprintf(outputFile, "    mov sp, x29\n");
    fprintf(outputFile, "    ldp x29, x30, [sp, -16]\n");
    fprintf(outputFile, "    ret\n");
}

static void genFunctionCallStmt(AST_NODE *functionCallStmtNode)
{
    fprintf(outputFile, "# Statement Function Call\n");
    genFunctionCallExpression(functionCallStmtNode);
    stackPop();
}

static void genAssignmentStmt(AST_NODE *assignmentStmtNode)
{
    fprintf(outputFile, "# Statement Assignment\n");
    genAssignmentExpression(assignmentStmtNode);
    stackPop();
}

static void genExpression(AST_NODE *exprNode)
{
    if (exprNode->nodeType == NUL_NODE) {
        return;
    } else if (exprNode->nodeType == STMT_NODE) {
        switch (exprNode->semantic_value.stmtSemanticValue.kind) {
            case ASSIGN_STMT: genAssignmentExpression(exprNode); break;
            case FUNCTION_CALL_STMT: genFunctionCallExpression(exprNode); break;
            default:
                fprintf(stderr, "Unsupported expression (line %d)\n", exprNode->linenumber);
        }
    } else if (exprNode->nodeType == CONST_VALUE_NODE) {
        genConstantExpression(exprNode);
    } else if (exprNode->nodeType == EXPR_NODE) {
        genArthimeticExpression(exprNode);
    } else if (exprNode->nodeType == IDENTIFIER_NODE) {
        genIdentifierExpression(exprNode);
    } else {
        fprintf(stderr, "Unsupported expression node type %d (line %d)\n", exprNode->nodeType, exprNode->linenumber);
    }
}

static void genArthimeticExpression(AST_NODE *exprNode)
{
    AST_NODE *leftOperand;
    AST_NODE *rightOperand;

    if (exprNode->dataType != INT_TYPE && exprNode->dataType != FLOAT_TYPE) {
        fprintf(stderr, "TypeError: The type of the expression is unknown. (line %d)\n", exprNode->linenumber);
        exit(1);
    }

    if (exprNode->semantic_value.exprSemanticValue.isConstEval) {
        /* 有常數值，直接使用 */
        switch (exprNode->dataType) {
            case INT_TYPE:
                stackPushIntegerConst(exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue);
                break;
            case FLOAT_TYPE:
                stackPushFloatConst(exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue);
                break;
            default:
                fprintf(stderr, "TypeError: The type of the expression value is unknown. (line %d)\n", exprNode->linenumber);
        }
        return;
    }

    if (exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION) {
        /* 處理子式 */
        leftOperand = exprNode->child;
        rightOperand = leftOperand->rightSibling;

        genExpression(leftOperand);
        genExpression(rightOperand);
    } else {
        leftOperand = exprNode->child;
        genExpression(leftOperand);
    }

    if (exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION) {
        /* 二元運算 */
        if (leftOperand->dataType == FLOAT_TYPE || rightOperand->dataType == FLOAT_TYPE) {
            /* 浮點數運算 */
            /* 讀取資料 */
            if (rightOperand->dataType == INT_TYPE) {
                stackTopInteger(INTEGER_TMP_REG2);
                genIntToFloat(INTEGER_TMP_REG2, FLOAT_TMP_REG2);
            } else {
                stackTopFloat(FLOAT_TMP_REG2);
            }
            stackPop();

            if (leftOperand->dataType == INT_TYPE) {
                stackTopInteger(INTEGER_TMP_REG);
                genIntToFloat(INTEGER_TMP_REG, FLOAT_TMP_REG);
            } else {
                stackTopFloat(FLOAT_TMP_REG);
            }
            stackPop();

            switch (exprNode->semantic_value.exprSemanticValue.op.binaryOp) {
            case BINARY_OP_ADD:
                fprintf(outputFile, "    fadd s%d, s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG, FLOAT_TMP_REG2);
                break;
            case BINARY_OP_SUB:
                fprintf(outputFile, "    fsub s%d, s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG, FLOAT_TMP_REG2);
                break;
            case BINARY_OP_MUL:
                fprintf(outputFile, "    fmul s%d, s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG, FLOAT_TMP_REG2);
                break;
            case BINARY_OP_DIV:
                fprintf(outputFile, "    fdiv s%d, s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG, FLOAT_TMP_REG2);
                break;
            case BINARY_OP_EQ:
                fprintf(outputFile, "    fcmp s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG2);
                fprintf(outputFile, "    cset x%d, eq\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_GE:
                fprintf(outputFile, "    fcmp s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG2);
                fprintf(outputFile, "    cset x%d, ge\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_LE:
                fprintf(outputFile, "    fcmp s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG2);
                fprintf(outputFile, "    cset x%d, le\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_NE:
                fprintf(outputFile, "    fcmp s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG2);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_GT:
                fprintf(outputFile, "    fcmp s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG2);
                fprintf(outputFile, "    cset x%d, gt\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_LT:
                fprintf(outputFile, "    fcmp s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG2);
                fprintf(outputFile, "    cset x%d, lt\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_AND:
                fprintf(outputFile, "    fcmp s%d, #0.0\n", FLOAT_TMP_REG);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    fcmp s%d, #0.0\n", FLOAT_TMP_REG2);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG2);
                fprintf(outputFile, "    and x%d, x%d, x%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
                break;
            case BINARY_OP_OR:
                fprintf(outputFile, "    fcmp s%d, #0.0\n", FLOAT_TMP_REG);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    fcmp s%d, #0.0\n", FLOAT_TMP_REG2);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG2);
                fprintf(outputFile, "    orr x%d, x%d, x%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
                break;
            }
        } else {
            /* 整數運算 */

            stackTopInteger(INTEGER_TMP_REG2);
            stackPop();

            stackTopInteger(INTEGER_TMP_REG);
            stackPop();

            switch (exprNode->semantic_value.exprSemanticValue.op.binaryOp) {
            case BINARY_OP_ADD:
                fprintf(outputFile, "    add w%d, w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
                break;
            case BINARY_OP_SUB:
                fprintf(outputFile, "    sub w%d, w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
                break;
            case BINARY_OP_MUL:
                fprintf(outputFile, "    mul w%d, w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
                break;
            case BINARY_OP_DIV:
                fprintf(outputFile, "    sdiv w%d, w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
                break;
            case BINARY_OP_EQ:
                fprintf(outputFile, "    cmp w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG2);
                fprintf(outputFile, "    cset x%d, eq\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_GE:
                fprintf(outputFile, "    cmp w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG2);
                fprintf(outputFile, "    cset x%d, ge\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_LE:
                fprintf(outputFile, "    cmp w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG2);
                fprintf(outputFile, "    cset x%d, le\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_NE:
                fprintf(outputFile, "    cmp w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG2);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_GT:
                fprintf(outputFile, "    cmp w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG2);
                fprintf(outputFile, "    cset x%d, gt\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_LT:
                fprintf(outputFile, "    cmp w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG2);
                fprintf(outputFile, "    cset x%d, lt\n", INTEGER_TMP_REG);
                break;
            case BINARY_OP_AND:
                fprintf(outputFile, "    cmp w%d, 0\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    cmp w%d, 0\n", INTEGER_TMP_REG2);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG2);
                fprintf(outputFile, "    and x%d, x%d, x%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
                break;
            case BINARY_OP_OR:
                fprintf(outputFile, "    cmp w%d, 0\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    cmp w%d, 0\n", INTEGER_TMP_REG2);
                fprintf(outputFile, "    cset x%d, ne\n", INTEGER_TMP_REG2);
                fprintf(outputFile, "    orr x%d, x%d, x%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
                break;
            }
        }
    } else {
        /* 一元運算 */
        if (leftOperand->dataType == FLOAT_TYPE) {
            /* 浮點數運算 */
            stackTopFloat(FLOAT_TMP_REG);
            stackPop();

            switch (exprNode->semantic_value.exprSemanticValue.op.unaryOp) {
            case UNARY_OP_POSITIVE: break;
            case UNARY_OP_NEGATIVE:
                fprintf(outputFile, "    fneg s%d, s%d\n", FLOAT_TMP_REG, FLOAT_TMP_REG);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                fprintf(outputFile, "    fcmp s%d, #0.0\n", FLOAT_TMP_REG);
                fprintf(outputFile, "    mov x%d, 1\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    moveq x%d, 0\n", INTEGER_TMP_REG);
                break;
            }
        } else {
            /* 整數運算 */
            stackTopInteger(INTEGER_TMP_REG);
            stackPop();

            switch (exprNode->semantic_value.exprSemanticValue.op.unaryOp) {
            case UNARY_OP_POSITIVE: break;
            case UNARY_OP_NEGATIVE:
                fprintf(outputFile, "    neg w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                fprintf(outputFile, "    cmp w%d, #0.0\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    mov x%d, 1\n", INTEGER_TMP_REG);
                fprintf(outputFile, "    moveq x%d, 0\n", INTEGER_TMP_REG);
                break;
            }
        }
    }

    if (exprNode->dataType == INT_TYPE) {
        stackPushIntegerReg(INTEGER_TMP_REG);
    } else {
        stackPushFloatReg(FLOAT_TMP_REG);
    }
}

static void genAssignmentExpression(AST_NODE *assignNode)
{
    AST_NODE *LValueNode = assignNode->child;
    AST_NODE *exprNode = LValueNode->rightSibling;
    SymbolTableEntry *symbol = LValueNode->semantic_value.identifierSemanticValue.symbolTableEntry;
    TypeDescriptor *typeDescriptor = symbol->attribute->attr.typeDescriptor;

    int offset = 0;

    genExpression(exprNode);

    if (typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR) {
        AST_NODE *dim = LValueNode->child;
        int dimension = typeDescriptor->properties.arrayProperties.dimension;
        genExpression(dim);

        for (int i = 1; i < dimension; i++) {

            stackPushIntegerConst(typeDescriptor->properties.arrayProperties.sizeInEachDimension[i]);
            stackTopInteger(INTEGER_TMP_REG2);
            stackPop();
            stackTopInteger(INTEGER_TMP_REG);
            stackPop();
            fprintf(outputFile, "    mul w%d, w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
            stackPushIntegerReg(INTEGER_TMP_REG);

            dim = dim->rightSibling;
            genExpression(dim);

            stackTopInteger(INTEGER_TMP_REG2);
            stackPop();
            stackTopInteger(INTEGER_TMP_REG);
            stackPop();
            fprintf(outputFile, "    add w%d, w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
            stackPushIntegerReg(INTEGER_TMP_REG);
        }
        stackTopInteger(INTEGER_TMP_REG);
        stackPop();
        fprintf(outputFile, "    lsl w%d, w%d, 2\n", INTEGER_TMP_REG, INTEGER_TMP_REG);
        stackPushIntegerReg(INTEGER_TMP_REG);
    }

    /* 計算位址 */
    if (symbol->global) {
        fprintf(outputFile, "    ldr x%d, =%s\n", INTEGER_TMP_REG2, symbol->name);
    } else {
        genLoadImmInteger64(INTEGER_TMP_REG2, symbol->offset);
        fprintf(outputFile, "    add x%d, x29, x%d\n", INTEGER_TMP_REG2, INTEGER_TMP_REG2);
    }

    if (typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR) {
        stackTopInteger(INTEGER_TMP_REG3);
        stackPop();
        fprintf(outputFile, "    add x%d, x%d, x%d\n", INTEGER_TMP_REG2, INTEGER_TMP_REG2, INTEGER_TMP_REG3);
    }

    if (exprNode->dataType == INT_TYPE) {
        stackTopInteger(INTEGER_TMP_REG);
    } else {
        stackTopFloat(FLOAT_TMP_REG);
    }

    if (assignNode->dataType == FLOAT_TYPE && exprNode->dataType == INT_TYPE) {
        genIntToFloat(INTEGER_TMP_REG, FLOAT_TMP_REG);
    }

    if (assignNode->dataType == INT_TYPE) {
        fprintf(outputFile, "    str w%d, [x%d]\n", INTEGER_TMP_REG, INTEGER_TMP_REG2);
    } else {
        fprintf(outputFile, "    str s%d, [x%d]\n", FLOAT_TMP_REG, INTEGER_TMP_REG2);
    }
}

static void genFunctionCallExpression(AST_NODE *functionCallNode)
{
    /* 目前先處理 function 最多只有一個 parameters */
    AST_NODE *idNode = functionCallNode->child;

    int parametersCount = 0;

    /* special case read, write */
    if (strcmp(idNode->semantic_value.identifierSemanticValue.identifierName, "write") == 0) {
        genWriteFuntionCallExpression(functionCallNode);
        return;
    }

    if (strcmp(idNode->semantic_value.identifierSemanticValue.identifierName, "read") == 0 ||
        strcmp(idNode->semantic_value.identifierSemanticValue.identifierName, "fread") == 0) {
        genReadFuntionCallExpression(functionCallNode);
        return;
    }

    SymbolTableEntry *symbol = idNode->semantic_value.identifierSemanticValue.symbolTableEntry;
    FunctionSignature* functionSignature = symbol->attribute->attr.functionSignature;
    parametersCount = functionSignature->parametersCount;

    if (parametersCount > 0) {
        /* 有參數要傳，只考慮一個參數就好，把參數放在 x0 */
        AST_NODE *paraExpr = idNode->rightSibling->child;
        genExpression(paraExpr);
        stackTopInteger(0);
        stackPop();
    }

    fprintf(outputFile, "    bl %s\n", idNode->semantic_value.identifierSemanticValue.identifierName);

    /* 回傳值放回 stack */
    stackPushIntegerReg(0);
}

static void genWriteFuntionCallExpression(AST_NODE *functionCallNode) {
    AST_NODE *exprNode = functionCallNode->child->rightSibling->child;

    genExpression(exprNode);

    if (exprNode->dataType == INT_TYPE) {
        stackTopInteger(0);
        fprintf(outputFile, "    bl _write_int\n");
    } else if (exprNode->dataType == FLOAT_TYPE) {
        stackTopFloat(0);
        fprintf(outputFile, "    bl _write_float\n");
    } else if (exprNode->dataType == CONST_STRING_TYPE) {
        stackTopInteger(0);
        fprintf(outputFile, "    bl _write_str\n");
    }
}

static void genReadFuntionCallExpression(AST_NODE *functionCallNode) {
    AST_NODE *idNode = functionCallNode->child;

    if (strcmp(idNode->semantic_value.identifierSemanticValue.identifierName, "read") == 0) {
        /* int read */
        fprintf(outputFile, "    bl _read_int\n");
        stackPushIntegerReg(0);
    } else {
        /* float read */
        fprintf(outputFile, "    bl _read_float\n");
        stackPushFloatReg(0);
    }
}



static void genIdentifierExpression(AST_NODE *identifierNode)
{
    SymbolTableEntry *symbol = identifierNode->semantic_value.identifierSemanticValue.symbolTableEntry;
    TypeDescriptor *typeDescriptor = symbol->attribute->attr.typeDescriptor;

    if (typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR) {
        AST_NODE *dim = identifierNode->child;
        int dimension = typeDescriptor->properties.arrayProperties.dimension;
        genExpression(dim);

        for (int i = 1; i < dimension; i++) {

            stackPushIntegerConst(typeDescriptor->properties.arrayProperties.sizeInEachDimension[i]);
            stackTopInteger(INTEGER_TMP_REG2);
            stackPop();
            stackTopInteger(INTEGER_TMP_REG);
            stackPop();
            fprintf(outputFile, "    mul w%d, w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
            stackPushIntegerReg(INTEGER_TMP_REG);

            dim = dim->rightSibling;
            genExpression(dim);

            stackTopInteger(INTEGER_TMP_REG2);
            stackPop();
            stackTopInteger(INTEGER_TMP_REG);
            stackPop();
            fprintf(outputFile, "    add w%d, w%d, w%d\n", INTEGER_TMP_REG, INTEGER_TMP_REG, INTEGER_TMP_REG2);
            stackPushIntegerReg(INTEGER_TMP_REG);
        }
        stackTopInteger(INTEGER_TMP_REG);
        stackPop();
        fprintf(outputFile, "    lsl w%d, w%d, 2\n", INTEGER_TMP_REG, INTEGER_TMP_REG);
        stackPushIntegerReg(INTEGER_TMP_REG);
    }

    /* 計算位址 */
    if (symbol->global) {
        fprintf(outputFile, "    ldr x%d, =%s\n", INTEGER_TMP_REG2, symbol->name);
    } else {
        genLoadImmInteger64(INTEGER_TMP_REG2, symbol->offset);
        fprintf(outputFile, "    add x%d, x29, x%d\n", INTEGER_TMP_REG2, INTEGER_TMP_REG2);
    }

    if (typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR) {
        stackTopInteger(INTEGER_TMP_REG3);
        stackPop();
        fprintf(outputFile, "    add x%d, x%d, x%d\n", INTEGER_TMP_REG2, INTEGER_TMP_REG2, INTEGER_TMP_REG3);
    }

    /* 載入資料進 stack */
    if (identifierNode->dataType == INT_TYPE) {
        fprintf(outputFile, "    ldr w%d, [x%d]\n", INTEGER_TMP_REG, INTEGER_TMP_REG2);
        stackPushIntegerReg(INTEGER_TMP_REG);
    } else {
        fprintf(outputFile, "    ldr s%d, [x%d]\n", FLOAT_TMP_REG, INTEGER_TMP_REG2);
        stackPushFloatReg(FLOAT_TMP_REG);
    }
}

static void genConstantExpression(AST_NODE *exprNode)
{
    if (exprNode->dataType == INT_TYPE) {
        stackPushIntegerConst(exprNode->semantic_value.const1->const_u.intval);
    } else if (exprNode->dataType == FLOAT_TYPE) {
        stackPushFloatConst(exprNode->semantic_value.const1->const_u.fval);
    } else { /* String */
        stackPushStringConst(exprNode->semantic_value.const1->const_u.sc);
    }
}

