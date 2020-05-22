#pragma warning(disable:4996)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#define MAXLEN 256
#define TBLSIZE 64
#define PRINTERR 1

typedef enum {
    UNKNOWN, END, ENDFILE,
    INT, ID,
    ADDSUB, MULDIV, LOGICAL,
    INCDEC, ASSIGN,
    LPAREN, RPAREN
} TokenSet;

// Test if a token matches the current token 
int match(TokenSet token);

// Get the next token
void advance(void);

// Get the lexeme of the current token
char* getLexeme(void);
#define error(errorNum) { \
    if (0) \
        fprintf(stderr, "error() called at %s:%d: ", __FILE__, __LINE__); \
    err(errorNum); \
}

// Error types
typedef enum {
    UNDEFINED, MISPAREN, NOTNUMID, NOTFOUND, RUNOUT, NOTLVAL, DIVZERO, SYNTAXERR
} ErrorType;

// Structure of the symbol table
typedef struct {
    int val;
    char name[MAXLEN];
} Symbol;

// Structure of a tree node
typedef struct _Node {
    TokenSet data;
    int val;
    int regnum;
    char lexeme[MAXLEN];
    struct _Node* left;
    struct _Node* right;
} BTNode;

// The symbol table
BTNode* retp = NULL;
Symbol table[TBLSIZE];
static TokenSet getToken(void);
static TokenSet curToken = UNKNOWN;
static char lexeme[MAXLEN];
int sbcount = 0;
Symbol table[TBLSIZE];
int reg[8] = { 0 };

//getmemory location
int getmem(char* str);

//lex function
// Initialize the symbol table with builtin variables
void initTable(void);

// Get the value of a variable
int getval(char* str);

// Set the value of a variable
int setval(char* str, int val);

// Make a new node according to token type and lexeme
BTNode* makeNode(TokenSet tok, const char* lexe);

// Free the syntax tree
void freeTree(BTNode* root);

BTNode* factor(void);
BTNode* term(void);
BTNode* term_tail(BTNode* left);
BTNode* expr(void);
BTNode* expr_tail(BTNode* left);
void statement(void);

// Print error message and exit the program
void err(ErrorType errorNum);

int findempt() {
    for (int i = 0; i < 8; i++) {
        if (reg[i] == 0) {
            reg[i] = 1;
            return i;
        }
    }
    error(RUNOUT);
}

//check has variable
bool checkvar(BTNode* root);



// Evaluate the syntax tree
int evaluateTree(BTNode* root);

//genrate code
int gencode(BTNode* root);

// Print the syntax tree in prefix
void printPrefix(BTNode* root);

int getmem(char* str) {
    for (int i = 0; i < sbcount; i++) {
        if (strcmp(str, table[i].name) == 0) {
            return i * 4;
        }
    }
}

bool checkvar(BTNode* root) {
    if (root != NULL) {
        bool b1 = checkvar(root->left);
        bool b2 = checkvar(root->right);
        bool b3 = (root->data == ID) ? true : false;
        return (b1 | b2 | b3);
    }
    return false;
}

TokenSet getToken(void) {
    int i = 0;
    char c = '\0';

    while ((c = fgetc(stdin)) == ' ' || c == '\t');

    if (isdigit(c)) {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isdigit(c) && i < MAXLEN) {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return INT;
    }
    else if (c == '+' || c == '-') {
        lexeme[0] = c;
        c = fgetc(stdin);
        if (c == lexeme[0]) {
            lexeme[1] = c;
            lexeme[2] = '\0';
            return INCDEC;
        }
        else {
            ungetc(c, stdin);
            lexeme[1] = '\0';
            return ADDSUB;
        }
    }
    else if (c == '&' || c == '|' || c == '^') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return LOGICAL;
    }
    else if (c == '*' || c == '/') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return MULDIV;
    }
    else if (c == '\n') {
        lexeme[0] = '\0';
        return END;
    }
    else if (c == '=') {
        strcpy(lexeme, "=");
        return ASSIGN;
    }
    else if (c == '(') {
        strcpy(lexeme, "(");
        return LPAREN;
    }
    else if (c == ')') {
        strcpy(lexeme, ")");
        return RPAREN;
    }
    else if (isalpha(c) || c == '_') {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isalpha(c) || isdigit(c) || c == '_') {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return ID;
    }
    else if (c == EOF) {
        return ENDFILE;
    }
    else {
        return UNKNOWN;
    }
}

void advance(void) {
    curToken = getToken();
}

int match(TokenSet token) {
    if (curToken == UNKNOWN)
        advance();
    return token == curToken;
}

char* getLexeme(void) {
    return lexeme;
}


void initTable(void) {
    strcpy(table[0].name, "x");
    table[0].val = 0;
    strcpy(table[1].name, "y");
    table[1].val = 0;
    strcpy(table[2].name, "z");
    table[2].val = 0;
    sbcount = 3;
}


int getval(char* str) {
    int i = 0;

    for (i = 0; i < sbcount; i++)
        if (strcmp(str, table[i].name) == 0)
            return table[i].val;

    error(NOTFOUND);
    return 0;
}

int setval(char* str, int val) {
    int i = 0;

    for (i = 0; i < sbcount; i++) {
        if (strcmp(str, table[i].name) == 0) {
            table[i].val = val;
            return val;
        }
    }

    if (sbcount >= TBLSIZE)
        error(RUNOUT);

    strcpy(table[sbcount].name, str);
    table[sbcount].val = val;
    sbcount++;
    return val;
}

BTNode* makeNode(TokenSet tok, const char* lexe) {
    BTNode* node = (BTNode*)malloc(sizeof(BTNode));
    strcpy(node->lexeme, lexe);
    node->data = tok;
    node->regnum = -1;
    node->val = 0;
    node->left = NULL;
    node->right = NULL;
    return node;
}

void freeTree(BTNode* root) {
    if (root != NULL) {
        freeTree(root->left);
        freeTree(root->right);
        free(root);
    }
}

// factor := INT | ADDSUB INT |
//		   	 ID  | ADDSUB ID  | 
//		   	 ID ASSIGN expr |
//           INCDEC ID||
//		   	 LPAREN expr RPAREN |
//		   	 ADDSUB LPAREN expr RPAREN
BTNode* factor(void) {
    BTNode* retp = NULL, * left = NULL;

    if (match(INT)) {
        retp = makeNode(INT, getLexeme());
        advance();
    }
    else if (match(ID)) {
        left = makeNode(ID, getLexeme());
        advance();
        if (!match(ASSIGN)) {
            retp = left;
        }
        else {
            retp = makeNode(ASSIGN, getLexeme());
            advance();
            retp->left = left;
            retp->right = expr();
        }
    }
    else if (match(ADDSUB)) {
        retp = makeNode(ADDSUB, getLexeme());
        retp->left = makeNode(INT, "0");
        advance();
        if (match(INT)) {
            retp->right = makeNode(INT, getLexeme());
            advance();
        }
        else if (match(ID)) {
            retp->right = makeNode(ID, getLexeme());
            advance();
        }
        else if (match(LPAREN)) {
            advance();
            retp->right = expr();
            if (match(RPAREN))
                advance();
            else
                error(MISPAREN);
        }
        else {
            error(NOTNUMID);
        }
    }
    else if (match(INCDEC)) {
        retp = makeNode(INCDEC, getLexeme());
        retp->right = makeNode(INT, "1");
        advance();
        if (match(ID)) {
            retp->left = makeNode(ID, getLexeme());
            advance();
        }
        else
            error(SYNTAXERR);
    }
    else if (match(LPAREN)) {
        advance();
        retp = expr();
        if (match(RPAREN))
            advance();
        else
            error(MISPAREN);
    }
    else {
        error(NOTNUMID);
    }
    return retp;
}

// term := factor term_tail
BTNode* term(void) {
    BTNode* node = factor();
    return term_tail(node);
}

// term_tail := MULDIV factor term_tail | NiL
BTNode* term_tail(BTNode* left) {
    BTNode* node = NULL;

    if (match(MULDIV)) {
        node = makeNode(MULDIV, getLexeme());
        advance();
        node->left = left;
        node->right = factor();
        return term_tail(node);
    }
    else {
        return left;
    }
}

// expr := term expr_tail
BTNode* expr(void) {
    BTNode* node = term();
    return expr_tail(node);
}

// expr_tail := ADDSUB_LOGICAL term expr_tail | NiL
BTNode* expr_tail(BTNode* left) {
    BTNode* node = NULL;

    if (match(ADDSUB)) {
        node = makeNode(ADDSUB, getLexeme());
        advance();
        node->left = left;
        node->right = term();
        return expr_tail(node);
    }
    else if (match(LOGICAL)) {
        node = makeNode(LOGICAL, getLexeme());
        advance();
        node->left = left;
        node->right = term();
        return expr_tail(node);
    }
    else {
        return left;
    }
}

// statement := ENDFILE | END | expr END
void statement(void) {
    if (match(ENDFILE)) {
        printf("MOV r0 [0]\n");
        printf("MOV r1 [4]\n");
        printf("MOV r2 [8]\n");
        printf("EXIT 0\n");
        exit(0);
    }
    else if (match(END)) {
         printf(">> ");
        advance();
    }
    else {
        retp = expr();
        if (match(END)) {
            printf("Ans: %d\n", evaluateTree(retp));
            //evaluateTree(retp);
            printf("Assembly Code:\n");
            gencode(retp);
            // printf("Prefix traversal: ");
             //printPrefix(retp);
             //printf("\n");
            freeTree(retp);
            printf(">> ");
            advance();
            for (int i = 0; i < 8; i++) reg[i] = 0;
        }
        else {
            error(SYNTAXERR);
        }
    }
}

void err(ErrorType errorNum) {
    if (PRINTERR) {
       // fprintf(stderr, "error: ");
        switch (errorNum) {
        case MISPAREN:
            fprintf(stderr, "mismatched parenthesis\n");
            printf("EXIT 1\n");
            break;
        case NOTNUMID:
            fprintf(stderr, "number or identifier expected\n");
            printf("EXIT 1\n");
            break;
        case NOTFOUND:
            fprintf(stderr, "variable not defined\n");
            printf("EXIT 1\n");
            break;
        case RUNOUT:
            fprintf(stderr, "out of memory\n");
            printf("EXIT 1\n");
            break;
        case NOTLVAL:
            fprintf(stderr, "lvalue required as an operand\n");
            printf("EXIT 1\n");
            break;
        case DIVZERO:
            fprintf(stderr, "divide by constant zero\n");
            printf("EXIT 1\n");
            break;
        case SYNTAXERR:
            fprintf(stderr, "syntax error\n");
            printf("EXIT 1\n");
            break;
        default:
            fprintf(stderr, "undefined error\n");
            printf("EXIT 1\n");
            break;
        }
    }
    exit(0);
}

int evaluateTree(BTNode* root) {
    int retval = 0, rv = 0, lv = 0;
    if (root != NULL)
    {
        if (root->data == ADDSUB || root->data == LOGICAL || root->data == MULDIV || root->data == INCDEC) {
            lv = evaluateTree(root->left); rv = evaluateTree(root->right);
            if (strcmp(root->lexeme, "+") == 0) {
                retval = lv + rv;
                root->val = retval;
            }
            else if (strcmp(root->lexeme, "-") == 0) {
                retval = lv - rv;
                root->val = retval;
            }
            else if (strcmp(root->lexeme, "*") == 0) {
                retval = lv * rv;
                root->val = retval;
            }
            else if (strcmp(root->lexeme, "/") == 0) {
                if (rv == 0) {//divide by 0
                    if (checkvar(root->right) == false) {
                        error(DIVZERO);
                    }
                    else {
                        retval = 0;
                        root->val = retval;
                    }

                }
                else {
                    retval = lv / rv;
                    root->val = retval;
                }
            }
            else if (strcmp(root->lexeme, "&") == 0) {
                retval = lv & rv;
                root->val = retval;
            }
            else if (strcmp(root->lexeme, "|") == 0) {
                retval = lv | rv;
                root->val = retval;
            }
            else if (strcmp(root->lexeme, "^") == 0) {
                retval = lv ^ rv;
                root->val = retval;
            }
            else if (strcmp(root->lexeme, "++") == 0) {
                retval = getval(root->left->lexeme) + 1;
                setval(root->left->lexeme, retval);
                root->val = retval;
            }
            else if (strcmp(root->lexeme, "--") == 0) {
                retval = getval(root->left->lexeme) - 1;
                setval(root->left->lexeme, retval);
                root->val = retval;
            }
        }
        else if (root->data == ASSIGN) {
            rv = evaluateTree(root->right);
            retval = rv;
            setval(root->left->lexeme, rv);
            root->val = rv;
        }
        else if (root->data == INT || root->data == ID) {
            if (root->data == INT) {
                root->val = atoi(root->lexeme);
                retval = root->val;
            }
            else {
                root->val = getval(root->lexeme);
                retval = root->val;
            }
        }
        else
            error(SYNTAXERR);
        //for (int i = 0; i < sbcount; i++) printf("%s = %d ", table[i].name,table[i].val); printf("\n");
        return retval;
    }
}

int gencode(BTNode* root) {
    if (root != NULL) {
        int lrg = 0, rrg = 0, retrg = 0;
        if (root->data == ADDSUB || root->data == LOGICAL || root->data == MULDIV) {
            lrg = gencode(root->left); rrg = gencode(root->right);
            if (strcmp(root->lexeme, "+") == 0) {
                printf("ADD r%d r%d\n", lrg, rrg);
                root->regnum = lrg;
                reg[rrg] = 0;
            }
            else if (strcmp(root->lexeme, "-") == 0) {
                printf("SUB r%d r%d\n", lrg, rrg);
                root->regnum = lrg;
                reg[rrg] = 0;
            }
            else if (strcmp(root->lexeme, "*") == 0) {
                printf("MUL r%d r%d\n", lrg, rrg);
                root->regnum = lrg;
                reg[rrg] = 0;
            }
            else if (strcmp(root->lexeme, "/") == 0) {
                printf("DIV r%d r%d\n", lrg, rrg);
                root->regnum = lrg;
                reg[rrg] = 0;
            }
            else if (strcmp(root->lexeme, "&") == 0) {
                printf("AND r%d r%d\n", lrg, rrg);
                root->regnum = lrg;
                reg[rrg] = 0;
            }
            else if (strcmp(root->lexeme, "|") == 0) {
                printf("OR r%d r%d\n", lrg, rrg);
                root->regnum = lrg;
                reg[rrg] = 0;
            }
            else if (strcmp(root->lexeme, "^") == 0) {
                printf("XOR r%d r%d\n", lrg, rrg);
                root->regnum = lrg;
                reg[rrg] = 0;
            }
            return lrg;
        }
        else if (root->data == INCDEC) {
            lrg = gencode(root->left); rrg = gencode(root->right);
            if (strcmp(root->lexeme, "++") == 0) {
                root->regnum = lrg;
                printf("ADD r%d r%d\n", lrg, rrg);
                printf("MOV [%d] r%d\n", getmem(root->left->lexeme), lrg);
                reg[rrg] = 0;
            }
            else {
                root->regnum = lrg;
                printf("SUB r%d r%d\n", lrg, rrg);
                printf("MOV [%d] r%d\n", getmem(root->left->lexeme), lrg);
                reg[rrg] = 0;
            }
            return root->regnum;
        }
        else if (root->data == ASSIGN) {
            if (root == retp) {
                rrg = gencode(root->right);
                printf("MOV [%d] r%d\n", getmem(root->left->lexeme), rrg);
                reg[rrg] = 0;
            }
            else {
                rrg = gencode(root->right);
                printf("MOV [%d] r%d\n", getmem(root->left->lexeme), rrg);
                reg[rrg] = 0;
                root->left->regnum = findempt();
                root->regnum = root->left->regnum;
                printf("MOV r%d [%d]\n", root->left->regnum, getmem(root->left->lexeme));
            }
            return root->regnum;
        }
        else if (root->data == ID || root->data == INT) {
            if (root->data == INT) {
                root->regnum = findempt();
                printf("MOV r%d %d\n", root->regnum, atoi(root->lexeme));
                return root->regnum;
            }
            else {
                root->regnum = findempt();
                printf("MOV r%d [%d]\n", root->regnum, getmem(root->lexeme));
                return root->regnum;
            }
        }
        else
            error(SYNTAXERR);
    }
}

void printPrefix(BTNode* root) {
    if (root != NULL) {
        printf("%s ", root->lexeme);
        printPrefix(root->left);
        printPrefix(root->right);
    }
}

int main() {
    initTable();
    printf(">> ");
    while (1) {
        statement();
    }
    return 0;
}