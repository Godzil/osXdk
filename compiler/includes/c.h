/* C compiler: definitions */
#ifndef C_H
#define C_H

#include "ops.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

/* default sizes */
#define MAXLINE   512      /* maximum output line length */
#define MAXTOKEN  32       /* maximum token length */
#define BUFSIZE	4096     /* input buffer size */
#define HASHSIZE  128      /* default hash table size */
#define MEMINCR   10	      /* blocks (1kb) allocated per arena */

#define va_init va_start

#define xx(a,b,c,d,e,f,g) a=b,
#define yy(a,b,c,d,e,f,g)

#define dclproto(func,args) func args

/* sType_t definitions */

typedef struct symbol *sSymbol_t; /* symbol table entries */
typedef struct table  *sTable_t;	 /* symbol tables */
typedef struct tynode *sType_t;	 /* type nodes */
typedef struct node   *sNode_t;	 /* dag nodes */
typedef struct tree   *sTree_t;	 /* tree nodes */
typedef struct field  *sField_t;	 /* struct/union fields */
typedef struct swtch  *sSwtch_t;	 /* switch data */

typedef void (*fApply_t)(void *, void *, void *);
typedef void (*fApplySymbol_t)(sSymbol_t, void *);

/* Some structures */

typedef union value 
{	/* constant values: */
	
   char sc;		/* signed */
	short ss;		/* signed */
	int i;			/* signed */
	unsigned char uc;
	unsigned short us;
	unsigned int u;
	float f;
	double d;
	char *p;		/* pointer to anything */
   
} uValue_t;

typedef struct list
{		/* lists: */
	void *x;			/* element */
	struct list *link;		/* next node */
} *sList_t;

typedef struct coord
{	/* source coordinates: */
   
	char *file;		/* file name */
	unsigned short x, y;	/* x,y position in file */
   
} sCoordinate_t;

typedef enum tokencode
{
#include "token.h"
	NTOKENS
} eTypeop_t;		/* type operators are a subset of tokens */

/* */

void address(sSymbol_t, sSymbol_t, int);
void asmcode(char *, sSymbol_t []);
void defaddress(sSymbol_t);
void defconst(int, uValue_t);
void defstring(int, char *);
void defsymbol(sSymbol_t);
void emit(sNode_t);
void export(sSymbol_t);
void function(sSymbol_t, sSymbol_t [], sSymbol_t [], int);
sNode_t gen(sNode_t);
void global(sSymbol_t);
void import(sSymbol_t);
void local(sSymbol_t);
void progbeg(int, char **);
void progend(void);
void segment(int);
void space(int);

/* symbol table */
void stabblock(int, int, sSymbol_t*);
void stabend(sCoordinate_t *, sSymbol_t, sCoordinate_t **, sSymbol_t *, sSymbol_t *);
void stabfend(sSymbol_t, int);
void stabinit(char *, int, char *[]);
void stabline(sCoordinate_t *);
void stabsym(sSymbol_t);
void stabtype(sSymbol_t);

#include "config.h"

#ifndef MAXKIDS
# define MAXKIDS 2
#endif

#ifndef MAXSYMS
# define MAXSYMS 3
#endif

#ifndef blockbeg
void blockbeg(sEnv_t *);
#endif

#ifndef blockend
void blockend(sEnv_t *);
#endif

#ifndef JUMP_ON_RETURN
# define JUMP_ON_RETURN 0
#endif

/* limits */
#ifdef __LCC__
# include <limits.h>
# include <float.h>
#else
/*
 * The magnitudes of the values below are greater than or equal to the minimum
 * permitted by the standard (see Appendix D) and are typical for byte-addressed
 * machines with 32-bit integers. These values are suitable for bootstrapping.
 */
# define CHAR_BIT	   8
# define MB_LEN_MAX	1

# define UCHAR_MAX	0xff
# define USHRT_MAX	0xffff
# define UINT_MAX	   0xffffffff
# define ULONG_MAX	0xffffffffL

# define CHAR_MAX	   SCHAR_MAX
# define SCHAR_MAX	0x7f
# define SHRT_MAX	   0x7fff
# define INT_MAX		0x7fffffff
# define LONG_MAX	   0x7fffffffL

# define CHAR_MIN	   SCHAR_MIN
# define SCHAR_MIN	(-SCHAR_MAX-1)
# define SHRT_MIN	   (-SHRT_MAX-1)
# define INT_MIN		(-INT_MAX-1)
# define LONG_MIN	   (-LONG_MAX-1)

# define FLT_MAX		1e37
# define DBL_MAX		1e37
#endif

/* data structures */

struct symbol                 /* symbol structures: */
{		
	sXsymbol_t x;              /* back-end's type extension */
   
	char *name;                /* name */
	unsigned short scope;      /* scope level */
	unsigned char sclass;      /* storage class */
	unsigned defined:1;        /* 1 if defined */
	unsigned temporary:1;      /* 1 if a temporary */
	unsigned generated:1;      /* 1 if a generated identifier */
	unsigned computed:1;       /* 1 if an address computation identifier */
	unsigned addressed:1;      /* 1 if its address is taken */
	unsigned initialized:1;    /* 1 if local is initialized */
	unsigned structarg:1;      /* 1 if parameter is a struct */
	int ref;                   /* weighted # of references */
	
   sType_t type;              /* data type */
	sCoordinate_t src;         /* definition coordinate */
	sCoordinate_t **uses;      /* array of Coordinate *'s for uses (omit) */
	sSymbol_t up;              /* next symbol in this or outer scope */
	
   union
   {
		struct
      {                       /* labels: */
			int label;           /* label number */
			sSymbol_t equatedto;	/* equivalent label */
		} l;
		
      struct
      {                       /* struct/union types: */
			unsigned cfields:1; 	/* 1 if >= 1 const fields */
			unsigned vfields:1;	/* 1 if >= 1 volatile fields */
			sTable_t ftab;       /* if xref != 0, table of field names */
			sField_t flist;      /* field list */
		} s;
		
      int value;              /* enumeration identifiers: value */
		sSymbol_t *idlist;      /* enumeration types: identifiers */
		
      struct 
      {                       /* constants: */
			uValue_t v;          /* value */
			sSymbol_t loc;       /* out-of-line location */
		} c;
		
      struct
      {                       /* functions: */
			sCoordinate_t pt[3]; /* source code coordinates */
			int label;           /* exit label */
			int ncalls;          /* # calls in this function */
			sSymbol_t *callee;	/* parameter symbols */
		} f;
		int seg;                /* globals, statics: definition segment */
	} u;
   
#ifdef Ysymbol                /* (omit) */
	sYsymbol_t y;              /* (omit) */
#endif                        /* (omit) */
};


typedef struct Xtype
{
	unsigned printed:1;
	unsigned marked:1;
	unsigned short typeno;
} sXtype_t;

enum { CODE=1, BSS, DATA, LIT, SYM };	/* logical segments */
enum { CONSTANTS=1, LABELS, GLOBAL, PARAM, LOCAL };


/* misc. macros */


#ifdef __WINDOWS__
/* C library */
# ifndef strtod
dclproto(extern double strtod,(char *, char **));
# endif

int atoi(char *);
int close(int);
int creat(char *, int);
void exit(int);
void * malloc(unsigned);
int open(char *, int);
int read(int, char *, int);
long strtol(char *, char **, int);
int sprintf(char *, const char *, ...);
char *strchr(const char *, int);
int strcmp(const char *, const char *);
unsigned strlen(const char *);
char *strncmp(const char *, const char *, unsigned);
char *strncpy(char *, const char *, unsigned);
int write(int, char *, int);

# define roundup(x,n) (((x)+((n)-1))&(~((n)-1)))
# define utod(x)	(2.*(int)((unsigned)(x)>>1)+(int)((x)&1))
# ifdef NDEBUG
#  define assert(c)
# else
#  define assert(c) ((c) || fatal(__FILE__,"assertion failure at line %d\n",\
__LINE__))
# endif

#else

#include <assert.h>

# define roundup(x,n) (((x)+((n)-1))&(~((n)-1)))
# define utod(x)	(2.*(int)((unsigned)(x)>>1)+(int)((x)&1))

#endif

struct node
{                                   /* dag nodes: */
	eOpcode_t op;                    /* operator */
	short count;                     /* reference count */
 	sSymbol_t syms[MAXSYMS];         /* symbols */
	sNode_t kids[MAXKIDS];           /* operands */
	sNode_t link;                    /* next dag in the forest */
	sXnode_t x;                      /* back-end's type extension */
};

#define islabel(p) ((p) && (p)->op == LABEL+V && (p)->syms[0])

struct tree
{                                   /* tree nodes: */
	eOpcode_t op;                    /* operator */
	sType_t type;                    /* type of result */
	sTree_t kids[2];                 /* operands */
	sNode_t node;                    /* associated dag node */
	union
   {
		sSymbol_t sym;                /* associated symbol */
		uValue_t v;                   /* associated value */
		sField_t field;               /* associated struct/union bit field */
	} u;
};

typedef struct arena
{                                   /* storage allocation arena: */
	int m;                           /* size of current allocation request */
	char *avail;                     /* next available location */
	char *limit;                     /* address 1 past end of arena */
	struct arena *first;             /* pointer to first arena */
	struct arena *next;              /* link to next arena */
} *sArena_t;

#define yyalloc(n,ap) (ap->m = roundup(n,sizeof(double)), \
	ap->avail + ap->m >= ap->limit ? allocate(ap->m, &ap) : \
	(ap->avail += ap->m, ap->avail - ap->m))
#define alloc(n)  yyalloc(n, permanent)
#define talloc(n) yyalloc(n, transient)
#define BZERO(p,t) \
	{ unsigned *q1 = (unsigned *)(p), *q2 = q1 + ((sizeof (t)/sizeof (unsigned))&~(8-1)); \
	for ( ; q1 < q2; q1 += 8) \
		q1[0] = q1[1] = q1[2] = q1[3] = q1[4] = q1[5] = q1[6] = q1[7] = 0; \
	if (sizeof (t)/sizeof (unsigned)%8 >= 1) q1[0] = 0; \
	if (sizeof (t)/sizeof (unsigned)%8 >= 2) q1[1] = 0; \
	if (sizeof (t)/sizeof (unsigned)%8 >= 3) q1[2] = 0; \
	if (sizeof (t)/sizeof (unsigned)%8 >= 4) q1[3] = 0; \
	if (sizeof (t)/sizeof (unsigned)%8 >= 5) q1[4] = 0; \
	if (sizeof (t)/sizeof (unsigned)%8 >= 6) q1[5] = 0; \
	if (sizeof (t)/sizeof (unsigned)%8 >= 7) q1[6] = 0; \
	if (sizeof (t)%sizeof (unsigned) >= 1)   ((char *)(q1 + sizeof (t)/sizeof (unsigned)%8))[0] = 0; \
	if (sizeof (t)%sizeof (unsigned) >= 2)   ((char *)(q1 + sizeof (t)/sizeof (unsigned)%8))[1] = 0; \
	if (sizeof (t)%sizeof (unsigned) >= 3)   ((char *)(q1 + sizeof (t)/sizeof (unsigned)%8))[2] = 0; \
	}

typedef struct code
{                                   /* code list entries: */
	enum
	{
		Blockbeg = 0, 
		Blockend, 
		Local, 
		Address, 
		Defpoint,
		Label, 
		Start, 
		Asm, 
		Gen, 
		Jump, 
		Switch
	} kind;

	struct code *prev;               /* previous code node */
   struct code *next;               /* next code node */
	union
	{
		struct
		{                             /* Asm: assembly language */
			char *code;                /* assembly code */
			sSymbol_t *argv;           /* %name arguments */
		} acode;
		
      struct
		{                             /* Blockbeg: */
			struct code *prev;         /* previous Blockbeg */
			short bnumber;             /* block number */
			short level;               /* block level */
			sSymbol_t *locals;         /* locals */
			sTable_t identifiers, types; /* symbol tables; used for -g */
			sEnv_t x;                  /* value filled in by blockbeg() */
		} block;
		
      sSymbol_t var;                /* Local: local variable */
		
      struct
		{                             /* Address: */
			sSymbol_t sym;             /* created symbol */
			sSymbol_t base;            /* local or parameter */
			int offset;                /* offset from sym */
		} addr;
      
		struct
		{                             /* Defpoint: execution point */
			sCoordinate_t src;         /* source location */
			int point;                 /* execution point number */
		} point;
      
		sNode_t node;                 /* Label, Gen, Jump: a dag node */
      
		struct swselect
		{                             /* Switch: swselect data */
			sSymbol_t sym;             /* temporary holding value */
			sSymbol_t table;           /* branch table */
			sSymbol_t deflab;          /* default label */
			int size;                  /* size of value & label arrays */
			int *values;               /* value, label pairs */
			sSymbol_t *labels;
		} swtch;
	} u;
} *Code;

struct tynode
{                                   /* type nodes: */
	eTypeop_t op;                    /* operator */
	short align;                     /* alignment in storage units */
	int size;                        /* size in storage units */
	sType_t type;                    /* operand */
	union
	{
		sSymbol_t sym;                /* associated symbol */
		sType_t *proto;               /* function prototype */
		void * ptr;
	} u;
	sXtype_t x;                      /* symbol table information */
#ifdef Ytype
	sYtype_t y;
#endif
};

typedef struct Event
{
	sList_t entry;
	sList_t exit;
	sList_t returns;
	sList_t points;
	sList_t calls;
	sList_t end;
} sEvents_t;

struct field
{                                   /* struct/union fields: */
	char *name;                      /* field name */
	sType_t type;                    /* data type */
	int offset;                      /* field offset */
	short from, to;                  /* bit fields: bits from..to */
	sField_t link;                   /* next field in this type */
};

#define fieldsize(p) ((p)->to - (p)->from)
#ifdef LITTLE_ENDIAN
#define fieldright(p) (p)->from
#else
#define fieldright(p) (8*(p)->type->size - (p)->to)
#endif
#define fieldmask(p) (~(~(unsigned)0<<fieldsize(p)))
#define fieldleft(p) (8*(p)->type->size - fieldsize(p) - fieldright(p))

/*
 * type-checking macros.
 * the operator codes are defined in token.h
 * to permit the range tests below; don't change them.
 */
#define isqual(t)	((t)->op >= CONST)
#define isvolatile(t)	((t)->op == VOLATILE || (t)->op == CONST+VOLATILE)
#define isconst(t)	((t)->op == CONST    || (t)->op == CONST+VOLATILE)
#define unqual(t)	(isqual(t) ? (t)->type : t)
#define isarray(t)	(unqual(t)->op == ARRAY)
#define isstruct(t)	(unqual(t)->op == STRUCT || unqual(t)->op == UNION)
#define isunion(t)	(unqual(t)->op == UNION)
#define isfunc(t)	(unqual(t)->op == FUNCTION)
#define isptr(t)	(unqual(t)->op == POINTER)
#define ischar(t)	(unqual(t)->op == CHAR)
#define isint(t)	(unqual(t)->op >= CHAR && unqual(t)->op <= UNSIGNED)
#define isfloat(t)	(unqual(t)->op <= DOUBLE)
#define isarith(t)	(unqual(t)->op <= UNSIGNED)
#define isunsigned(t)	(unqual(t)->op == UNSIGNED)
#define isdouble(t)	(unqual(t)->op == DOUBLE)
#define isscalar(t)	(unqual(t)->op <= POINTER || unqual(t)->op == ENUM)
#define isenum(t)	(unqual(t)->op == ENUM)
#define widen(t)	(isint(t) || isenum(t) ? INT : ttob(t))

extern struct code codehead;
extern Code codelist;
extern int nodecount;
extern sEvents_t events;
extern sSymbol_t cfunc;
extern char *fname;
extern sSymbol_t retv;
extern int ntree;
extern unsigned char *cp;
extern char *file;
extern char *firstfile;
extern unsigned char *limit;
extern char *line;
extern int lineno;
extern char kind[];
extern sCoordinate_t src;
extern enum tokencode t;
extern char *token;
extern sSymbol_t tsym;
extern int errcnt;
extern int errlimit;
extern int wflag;
extern int ncalled;
extern int npoints;
extern int Aflag;
extern int Pflag;
extern sSymbol_t YYnull;
extern int glevel;
extern int xref;
extern int needconst;
extern float density;
extern int refinc;
extern sArena_t permanent;
extern sArena_t transient;
extern int bnumber;
extern sTable_t constants;
extern sTable_t externals;
extern sTable_t globals;
extern sTable_t identifiers;
extern sTable_t labels[2];
extern sTable_t types;
extern int level;
extern sType_t chartype;
extern sType_t doubletype;
extern sType_t floattype;
extern sType_t inttype;
extern sType_t longdouble;
extern sType_t longtype;
extern sType_t shorttype;
extern sType_t signedchar;
extern sType_t unsignedchar;
extern sType_t unsignedlong;
extern sType_t unsignedshort;
extern sType_t unsignedtype;
extern sType_t voidptype;
extern sType_t voidtype;
extern sList_t symbols;
extern char *bp;

void        addlocal(sSymbol_t);
Code        code(int);
void        emitcode(void);
void        gencode(sSymbol_t [], sSymbol_t []);
sNode_t     listnodes(sTree_t, int, int);
sNode_t     jump(int);
sNode_t     newnode(int, sNode_t, sNode_t, sSymbol_t);
sNode_t     node(int, sNode_t, sNode_t, sSymbol_t);
void        printdag(sNode_t, int);
void        walk(sTree_t, int, int);
void        compound(int, sSwtch_t, int);
void        finalize(void);
void        program(void);
sType_t     typename(void);
int         genconst(sTree_t, int);
int         hascall(sTree_t);
int         nodeid(sTree_t);
char *      opname(int);
int *       printed(int);
void        printtree(sTree_t, int);
sTree_t     retype(sTree_t, sType_t);
sTree_t     root(sTree_t);
sTree_t     texpr(sTree_t (*)(int), int);
void        tfree(void);
sTree_t     tree(int, sType_t, sTree_t, sTree_t);
sTree_t     addrof(sTree_t);
sTree_t     asgn(sSymbol_t, sTree_t);
sType_t     assign(sType_t, sTree_t);
sTree_t     cast(sTree_t, sType_t);
sTree_t     cond(sTree_t);
sTree_t     conditional(int);
sTree_t     constexpr(int);
sTree_t     expr0(int);
sTree_t     expr(int);
sTree_t expr1(int);
sTree_t field(sTree_t, char *);
char *funcname(sTree_t);
sTree_t idnode(sSymbol_t);
sTree_t incr(int, sTree_t, sTree_t);
int intexpr(int, int);
sTree_t lvalue(sTree_t);
sTree_t pointer(sTree_t);
sType_t promote(sType_t);
sTree_t right(sTree_t, sTree_t);
sTree_t rvalue(sTree_t);
sTree_t cvtconst(sTree_t);
void defglobal(sSymbol_t, int);
void defpointer(sSymbol_t);
void doconst(sSymbol_t, void *);
void initglobal(sSymbol_t, int);
sType_t initializer(sType_t, int);
sTree_t structexp(sType_t, sSymbol_t);
void swtoseg(int);
void inputInit(int);
void inputstring(char *);
void fillbuf(void);
void nextline(void);
int getchr(void);
int gettok(void);
int main(int, char **);
sSymbol_t mkstr(char *);
sSymbol_t mksymbol(int, char *,sType_t);
void bbinit(char *);
void traceinit(char *);
void attach(fApply_t, void *, sList_t *);
void apply(sList_t, void *, void *);
void fprint(int, char *, ...);
void print(char *, ...);
char *stringf(char *, ...);
void outflush(void);
void outs(char *);
void vfprint(int, char *, va_list);
void vprint(char *, va_list);
void error(char *, ...);
int fatal(char *, char *, int);
void warning(char *, ...);
int expect(int);
void skipto(int, char *);
void test(int, char *);
int process(char *);
int findfunc(char *, char *);
int findcount(char *, int, int);
sTree_t asgnnode(int, sTree_t, sTree_t);
sTree_t bitnode(int, sTree_t, sTree_t);
sTree_t callnode(sTree_t, sType_t, sTree_t);
sTree_t condnode(sTree_t, sTree_t, sTree_t);
sTree_t constnode(unsigned int, sType_t);
sTree_t eqnode(int, sTree_t, sTree_t);
sTree_t shnode(int, sTree_t, sTree_t);
void typeerror(int, sTree_t, sTree_t);

sTree_t (*opnode[512])(int, sTree_t, sTree_t);

sTree_t simplify(int, sType_t, sTree_t, sTree_t);
int ispow2(unsigned u);
char *vtoa(sType_t, uValue_t);
void definelab(int);
Code definept(sCoordinate_t *);
void equatelab(sSymbol_t, sSymbol_t);
void flushequ(void);
void retcode(sTree_t, int);
void statement(int, sSwtch_t, int);
char *allocate(int, sArena_t *);
void deallocate(sArena_t *);
sList_t append(void *, sList_t);
int length(sList_t);
void * *ltoa(sList_t, void * []);
char *string(char *);
char *stringd(int);
char *stringn(char *, int);
sSymbol_t constant(sType_t, uValue_t);
void enterscope(void);
void exitscope(void);
void fielduses(sSymbol_t, void *);
sSymbol_t findlabel(int);
sSymbol_t findtype(sType_t);

void foreach(sTable_t, int, void (*)(sSymbol_t, void *), void *);


sSymbol_t genident(int, sType_t, int);
int genlabel(int);
sSymbol_t install(char *, sTable_t *, int);
sSymbol_t intconst(int);
void locus(sTable_t, sCoordinate_t *);
sSymbol_t lookup(char *, sTable_t);
sSymbol_t newconst(uValue_t v,int tc);
sSymbol_t newtemp(int, int);
void rmtemps(int, int);
void release(sSymbol_t);
void setuses(sTable_t);
sTable_t table(sTable_t, int);
sSymbol_t temporary(int, sType_t);
void use(sSymbol_t, sCoordinate_t);
void typeInit(void);
sType_t array(sType_t, int, int);
sType_t atop(sType_t);
void checkfields(sType_t);
sType_t composite(sType_t, sType_t);
sSymbol_t deftype(char *, sType_t, sCoordinate_t *);
sType_t deref(sType_t);
int eqtype(sType_t, sType_t, int);
sField_t extends(sType_t, sType_t);
sField_t fieldlist(sType_t);
sField_t fieldref(char *, sType_t);
sType_t freturn(sType_t);
sType_t func(sType_t, sType_t *);
int hasproto(sType_t);
sField_t newfield(char *, sType_t, sType_t);
sType_t newstruct(int, char *);
void outtype(sType_t);
void printdecl(sSymbol_t, sType_t);
void printproto(sSymbol_t, sSymbol_t *);
void printtype(sType_t, int);
sType_t ptr(sType_t);
sType_t qual(int, sType_t);
void rmtypes(void);
int ttob(sType_t);
char *typestring(sType_t, char *);
int variadic(sType_t);

#endif	
