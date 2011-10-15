/* C compiler: main program */
#include "version.h"
#include "c.h"

static void compile(char *);
static int doargs(int,char **);
static void emitYYnull();
static sType_t ftype(sType_t,sType_t);
static void typestab(sSymbol_t, void *);


int Aflag;			/* >= 0 if -A specified */
int Pflag;			/* != 0 if -P specified */
int glevel;			/* == [0-9] if -g[0-9] specified */
int xref;			/* != 0 for cross-reference data */
sSymbol_t YYnull;			/* symbol for _YYnull if -n specified */

static char *infile;		/* input file */
static char *outfile;		/* output file */
static char *progname;		/* argv[0] */

sList_t loci, tables;		/* current (locus,table) lists */



int main(int argc,char *argv[]) 
{
	sSymbol_t symroot = 0;		/* root of the global symbols */
	static sEvents_t z;
	
	assert(MAXKIDS >= 2);
	assert(MAXSYMS >= 2);
	progname = argv[0];
	typeInit();
	level = GLOBAL;
	argc = doargs(argc, argv);
	assert(inttype->size >= voidptype->size);
	if (infile && *infile != '-') {
		close(0);
		if (open(infile, 0) != 0) {
			fprint(2, "%s: can't read %s\n", argv[0], infile);
			exit(1);
		}
	}
	if (outfile && *outfile != '-') {
		close(1);
		if (creat(outfile, 0666) != 1) {
			fprint(2, "%s: can't write %s\n", argv[0], outfile);
			exit(1);
		}
	}
	inputInit(0);
	t = gettok();
	progbeg(argc, argv);
	stabinit(firstfile, argc, argv);
	program();
	if (events.end)
		apply(events.end, (void *)0, (void *)0);
	events = z;
	emitYYnull();
	finalize();
	if (glevel || xref) {
		sCoordinate_t src;
		foreach(types, GLOBAL, typestab, (void *)&symroot);
		foreach(identifiers, GLOBAL, typestab, (void *)&symroot);
		src.file = firstfile;
		src.x = 0;
		src.y = lineno;
		if (glevel > 2 || xref)
			stabend(&src, symroot, (Coordinate **)ltoa(append((void *)0, loci), 0),
			(Symbol *)ltoa(append((void *)0, tables), 0),
			symbols ? (Symbol *)ltoa(symbols, 0) : 0);
		else
			stabend(&src, 0, 0, 0, 0);
	}
#ifdef progend
	progend(0);
#else
	progend();
#endif
	outflush();
	exit(errcnt > 0);
	return 0;
}

/* compile - compile str */
static void compile(str) char *str; {
	inputstring(str);
	t = gettok();
	program();
}

/* doargs - process program arguments, removing top-half arguments from argv */
static int doargs(argc, argv) char *argv[]; 
{
	char *s;
	int i, j, x;
	sSymbol_t p;
	
	for (i = j = 1; i < argc; i++)
		if (strcmp(argv[i], "-g") == 0)
			glevel = 2;
		else 
		if (strncmp(argv[i], "-g", 2) == 0 && argv[i][2] && argv[i][2] >= '0' && argv[i][2] <= '9')
			glevel = argv[i][2] - '0';
		else
		if (strcmp(argv[i], "-x") == 0)
			xref++;
		else
		if (strcmp(argv[i], "-A") == 0)
			Aflag++;
		else
		if (strcmp(argv[i], "-P") == 0)
			Pflag++;
		else
		if (strcmp(argv[i], "-w") == 0)
			wflag++;
		else
		if (strcmp(argv[i], "-b") == 0 || strcmp(argv[i], "-C") == 0 || strncmp(argv[i], "-a", 2) == 0)
			bbinit(argv[i]);
		else 
		if (strcmp(argv[i], "-n") == 0) 
		{
			if (!YYnull) 
			{
				YYnull = mksymbol(EXTERN, "_YYnull", ftype(voidtype, inttype));
				YYnull->sclass = STATIC;
			}
		} 
		else 
		if (strncmp(argv[i], "-t", 2) == 0)
			traceinit(&argv[i][2]);
		else 
		if (strcmp(argv[i], "-v") == 0)
			fprint(2, "%s version %d.%d\n", progname, VERSION>>8, VERSION&0xff);
		else 
		if (strncmp(argv[i], "-s", 2) == 0)
			density = (float)strtod(&argv[i][2], (char **)0);
		else 
		if (strncmp(argv[i], "-e", 2) == 0) 
		{
			if ((x = strtol(&argv[i][2], (char **)0, 0)) > 0)
				errlimit = x;
		} 
		else 
		if ((s = strchr(argv[i], '=')) &&
			(p = lookup(stringn(argv[i], s - argv[i]), types))) 
		{
			if (*s == '=') 
			{
				if ((x = strtol(s + 1, &s, 0)) > 0)
					p->type->size = x;
			}
			if (*s == ',') 
			{
				if ((x = strtol(s + 1, &s, 0)) > 0)
					p->type->align = x;
			}
			if (*s++ == ',')
				p->addressed = !(*s == 0 || *s == '0');
		} 
		else 
		if (strcmp(argv[i], "-") == 0 || *argv[i] != '-') 
		{
			if (infile == 0)
				infile = argv[i];
			else if (outfile == 0)
				outfile = argv[i];
			else
				argv[j++] = argv[i];
		} 
		else 
		{
			if (strcmp(argv[i], "-XP") == 0)
				argv[i] = "-p";
			else if (strncmp(argv[i], "-X", 2) == 0)
				*++argv[i] = '-';
			argv[j++] = argv[i];
		}
		argv[j] = 0;
		return j;
}


/* emitYYnull - compile definition for _YYnull, if referenced */
static void emitYYnull() 
{
	if (YYnull && YYnull->ref > 0) 
	{
		Aflag = 0;
		YYnull->defined = 0;
		YYnull = 0;
		compile(stringf("static char *_YYfile = \"%s\";\n", file));
		compile("static void _YYnull(int line,...) {\n\
				char buf[200];\n\
				sprintf(buf, \"null pointer dereferenced @%s:%d\\n\", _YYfile, line);\n\
				write(2, buf, strlen(buf));\n\
				abort();\n\
	}\n");
	} else if (YYnull)
		YYnull->ref = 1000;
}

/* ftype - return a function type for `rty function (ty,...)' */
static sType_t ftype(rty, ty) sType_t rty, ty; {
	sList_t list = append(ty, 0);
	
	list = append(voidtype, list);
	return func(rty, (sType_t *)ltoa(list, (void * *)alloc((length(list) + 1)*sizeof (sType_t))));
}

/* mkstr - make a string constant */
sSymbol_t mkstr(str) char *str; {
	uValue_t v;
	sSymbol_t p;
	
	v.p = str;
	p = constant(array(chartype, strlen(v.p) + 1, 0), v);
	if (p->u.c.loc == 0)
		p->u.c.loc = genident(STATIC, p->type, GLOBAL);
	return p;
}

/* mksymbol - make a symbol for name, install in &globals if sclass==EXTERN */
sSymbol_t mksymbol(sclass, name, ty) char *name; sType_t ty; {
	sSymbol_t p;
	
	if (sclass == EXTERN)
		p = install(string(name), &globals, 1);
	else {
		p = (sSymbol_t)alloc(sizeof *p);
		BZERO(p, struct symbol);
		p->name = string(name);
		p->scope = GLOBAL;
	}
	p->sclass = sclass;
	p->type = ty;
	defsymbol(p);
	p->defined = 1;
	return p;
}

/* typestab - emit stab entries for p */
static void typestab(p, cl) sSymbol_t p; void *cl; {
	if (*(sSymbol_t *)cl == 0 && p->sclass && p->sclass != TYPEDEF)
		*(sSymbol_t *)cl = p;
	if (p->sclass == TYPEDEF || p->sclass == 0)
		stabtype(p);
}

struct callsite {
	char *file, *name;
	union coordinate {
		struct {
#ifdef CC_LITTLE_ENDIAN
			unsigned int y:16,x:10,index:6;
#else
			unsigned int index:6,x:10,y:16;
#endif
		} c;
		unsigned int coord;
	} u;
};
struct func {
	struct func *link;
	struct caller *callers;
	char *name;
	union coordinate src;
};
struct map {		/* source code map; 200 coordinates/map */
	int size;
	union coordinate u[200];
};
int npoints;		/* # of execution points if -b specified */
int ncalled = -1;	/* #times prof.out says current function was called */
static sSymbol_t YYlink;	/* symbol for file's struct _bbdata */
static sSymbol_t YYcounts;	/* symbol for _YYcounts if -b specified */
static sList_t maplist;	/* list of struct map *'s */
static sList_t filelist;	/* list of file names */
static sSymbol_t funclist;	/* list of struct func *'s */
static sSymbol_t afunc;	/* current function's struct func */

static void bbcall(sSymbol_t, sCoordinate_t *, sTree_t *);
static void bbentry(sSymbol_t, sSymbol_t);
static void bbexit(sSymbol_t, sSymbol_t, sTree_t);
static int bbfile(char *);
static void bbfunc(sSymbol_t, sSymbol_t);
static void bbincr(sSymbol_t, sCoordinate_t *, sTree_t *);
static void bbvars(sSymbol_t);

/* bbcall - build tree to set _callsite at call site *cp, emit call site data */
static void bbcall(yycounts, cp, e) sSymbol_t yycounts; sCoordinate_t *cp; sTree_t *e; {
	static sSymbol_t caller;
	uValue_t v;
	union coordinate u;
	sSymbol_t p = genident(STATIC, array(voidptype, 0, 0), GLOBAL);
	
	defglobal(p, LIT);
	defpointer(cp->file ? mkstr(cp->file)->u.c.loc : 0);
	defpointer(mkstr(cfunc->name)->u.c.loc);
	u.c.x = cp->x;
	u.c.y = cp->y;
	defconst(U, (v.u = u.coord, v));
	if (caller == 0)
		caller = mksymbol(EXTERN, "_caller", ptr(voidptype));
	*e = right(asgn(caller, idnode(p)), *e);
}

/* bbentry - return tree for `_prologue(&afunc, &YYlink)' */
static void bbentry(yylink, f) sSymbol_t yylink, f; {
	static sSymbol_t p;
	
	afunc = genident(STATIC, array(voidptype, 4, 0), GLOBAL);
	if (p == 0)
		p = mksymbol(EXTERN, "_prologue", ftype(inttype, voidptype));
	walk(callnode(pointer(idnode(p)), freturn(p->type),
		tree(ARG+P, ptr(unsignedtype), idnode(yylink),
		tree(ARG+P, ptr(unsignedtype), idnode(afunc), 0))), 0, 0);
}

/* bbexit - return tree for `_epilogue(&afunc)' */
static void bbexit(yylink, f, e) sSymbol_t yylink, f; sTree_t e; {
	static sSymbol_t p;
	
	if (p == 0)
		p = mksymbol(EXTERN, "_epilogue", ftype(inttype, voidptype));
	walk(callnode(pointer(idnode(p)), freturn(p->type),
		tree(ARG+P, ptr(unsignedtype), idnode(afunc), 0)), 0, 0);
}

/* bbfile - add file to list of file names, return its index */
static int bbfile(file) char *file; {
	if (file) {
		sList_t lp;
		int i = 1;
		if (lp = filelist)
			do {
				lp = lp->link;
				if (((sSymbol_t)lp->x)->u.c.v.p == file)
					return i;
				i++;
			} while (lp != filelist);
			filelist = append(mkstr(file), filelist);
			return i;
	}
	return 0;
}

/* bbfunc - emit function name and src coordinates */
static void bbfunc(yylink, f) sSymbol_t yylink, f; {
	uValue_t v;
	union coordinate u;
	
	defglobal(afunc, DATA);
	defpointer(funclist);
	defpointer(0);
	defpointer(mkstr(f->name)->u.c.loc);
	u.c.x = f->u.f.pt[2].x;
	u.c.y = f->u.f.pt[2].y;
	u.c.index = bbfile(f->u.f.pt[2].file);
	defconst(U, (v.u = u.coord, v));
	funclist = afunc;
}

/* bbincr - build tree to increment execution point at *cp */
static void bbincr(yycounts, cp, e) sSymbol_t yycounts; sCoordinate_t *cp; sTree_t *e; {
	struct map *mp = (struct map *)maplist->x;
	
	/* append *cp to source map */
	if (mp->size >= sizeof mp->u/sizeof mp->u[0]) {
		mp = (struct map *)alloc(sizeof *mp);
		mp->size = 0;
		maplist = append((void * *)mp, maplist);
	}
	mp->u[mp->size].c.x = cp->x;
	mp->u[mp->size].c.y = cp->y;
	mp->u[mp->size++].c.index = bbfile(cp->file);
	*e = right(incr('+', rvalue((*opnode['+'])(ADD, pointer(idnode(yycounts)),
		constnode(npoints++, inttype))), constnode(1, inttype)), *e);
}

/* bbinit - initialize basic block counting options */
void bbinit(opt) char *opt; {
	if (strncmp(opt, "-a", 2) == 0) {
		if (ncalled == -1 && process(opt[2] ? &opt[2] : "prof.out") > 0)
			ncalled = 0;
	} else if ((strcmp(opt, "-b") == 0 || strcmp(opt, "-C") == 0) && YYlink == 0) {
		YYlink = genident(STATIC, array(unsignedtype, 0, 0), GLOBAL);
		attach((fApply_t)bbentry, (void *)YYlink, &events.entry);
		attach((fApply_t)bbexit,  (void *)YYlink, &events.returns);
		attach((fApply_t)bbfunc,  (void *)YYlink, &events.exit);
		attach((fApply_t)bbvars,  (void *)YYlink, &events.end);
		if (strcmp(opt, "-b") == 0) {
			YYcounts = genident(STATIC, array(unsignedtype, 0, 0), GLOBAL);
			maplist = append((void *)alloc(sizeof(struct map)), maplist);
			((struct map *)maplist->x)->size = 0;
			attach((fApply_t)bbcall, (void *)YYcounts, &events.calls);
			attach((fApply_t)bbincr, (void *)YYcounts, &events.points);
		}
	}
}

/* bbvars - emit definition for basic block counting data */
static void bbvars(yylink) sSymbol_t yylink; {
	int i, j, n = npoints;
	uValue_t v;
	struct map **mp;
	sSymbol_t coords, files, *p;
	
	if (!YYcounts && !yylink)
		return;
	if (YYcounts) {
		if (n <= 0)
			n = 1;
		defglobal(YYcounts, BSS);
		space(n*YYcounts->type->type->size);
	}
	files = genident(STATIC, array(ptr(chartype), 1, 0), GLOBAL);
	defglobal(files, LIT);
	for (p = (sSymbol_t *)ltoa(filelist, 0); *p; p++)
		defpointer((*p)->u.c.loc);
	defpointer(0);
	coords = genident(STATIC, array(unsignedtype, n, 0), GLOBAL);
	defglobal(coords, LIT);
	for (i = n, mp = (struct map **)ltoa(maplist, 0); *mp; i -= (*mp)->size, mp++)
		for (j = 0; j < (*mp)->size; j++)
			defconst(U, (v.u = (*mp)->u[j].coord, v));
		if (i > 0)
			space(i*coords->type->type->size);
		defpointer(0);
		defglobal(yylink, DATA);
		defpointer(0);
		defconst(U, (v.u = n, v));
		defpointer(YYcounts);
		defpointer(coords);
		defpointer(files);
		defpointer(funclist);
}

static char *fmt, *fp, *fmtend;	/* format string, current & limit pointer */
static sTree_t args;		/* printf arguments */
static sSymbol_t frameno;		/* local holding frame number */

static void appendstr(char *);
static void tracecall(sSymbol_t, sSymbol_t);
static void tracefinis(sSymbol_t);
static void tracereturn(sSymbol_t, sSymbol_t, sTree_t);
static void tracevalue(sTree_t, int);

/* appendstr - append str to the evolving format string, expanding it if necessary */
static void appendstr(str) char *str; {
	do
	if (fp == fmtend)
		if (fp) {
			char *s = (char *)talloc(2*(fmtend - fmt));
			strncpy(s, fmt, fmtend - fmt);
			fp = s + (fmtend - fmt);
			fmtend = s + 2*(fmtend - fmt);
			fmt = s;
		} else {
			fp = fmt = (char *)talloc(80);
			fmtend = fmt + 80;
		}
		while (*fp++ = *str++);
		fp--;
}

/* tracecall - generate code to trace entry to f */
static void tracecall(printer, f) sSymbol_t printer, f; {
	int i;
	sSymbol_t counter = genident(STATIC, inttype, GLOBAL);
	
	defglobal(counter, BSS);
	space(counter->type->size);
	frameno = genident(AUTO, inttype, level);
	addlocal(frameno);
	appendstr(f->name); appendstr("#");
	tracevalue(asgn(frameno, incr(INCR, idnode(counter), constnode(1, inttype))), 0);
	appendstr("(");
	for (i = 0; f->u.f.callee[i]; i++) {
		if (i)
			appendstr(",");
		appendstr(f->u.f.callee[i]->name); appendstr("=");
		tracevalue(idnode(f->u.f.callee[i]), 0);
	}
	if (variadic(f->type))
		appendstr(",...");
	appendstr(") called\n");
	tracefinis(printer);
}

/* tracefinis - complete & generate the trace call to print */
static void tracefinis(printer) sSymbol_t printer; {
	sTree_t *ap;
	sSymbol_t p;
	
	*fp = 0;
	p = mkstr(string(fmt));
	for (ap = &args; *ap; ap = &(*ap)->kids[1])
		;
	*ap = tree(ARG+P, ptr(chartype), pointer(idnode(p->u.c.loc)), 0);
	walk(callnode(pointer(idnode(printer)), freturn(printer->type), args), 0, 0);
	args = 0;
	fp = fmtend = 0;
}

/* traceinit - initialize for tracing */
void traceinit(print) char *print; {
	static sSymbol_t printer;
	
	if (!printer) {
		printer = mksymbol(STATIC, print && *print ? print : "printf",
			ftype(voidtype, ptr(chartype)));
		printer->sclass = EXTERN;
		attach((fApply_t)tracecall,   (void *)printer, &events.entry);
		attach((fApply_t)tracereturn, (void *)printer, &events.returns);
	}
}

/* tracereturn - generate code to trace return e */
static void tracereturn(printer, f, e) sSymbol_t printer, f; sTree_t e; {
	appendstr(f->name); appendstr("#");
	tracevalue(idnode(frameno), 0);
	appendstr(" returned");
	if (freturn(f->type) != voidtype && e) {
		appendstr(" ");
		tracevalue(e, 0);
	}
	appendstr("\n");
	tracefinis(printer);
}

/* tracevalue - append format and argument to print the value of e */
static void tracevalue(e, lev) sTree_t e; {
	sType_t ty = unqual(e->type);
	
	switch (ty->op) {
	case CHAR:
		appendstr("'\\x%2x'");
		break;
	case SHORT:
		if (ty == unsignedshort)
			appendstr("0x%x");
		else /* fall thru */
	case INT:
		appendstr("%d");
		break;
	case UNSIGNED:
		appendstr("0x%x");
		break;
	case FLOAT: case DOUBLE:
		appendstr("%g");
		break;
	case POINTER:
		if (unqual(ty->type) == chartype) {
			static sSymbol_t null;
			if (null == 0)
				null = mkstr("(null)");
			tracevalue(constnode(0, unsignedtype), lev + 1);
			appendstr(" \"%s\"");
			e = condnode(e, e, pointer(idnode(null->u.c.loc)));
		} else {
			appendstr("("); appendstr(typestring(ty, "")); appendstr(")0x%x");
		}
		break;
	case STRUCT: {
		sField_t q;
		appendstr("("); appendstr(typestring(ty, "")); appendstr("){");
		for (q = ty->u.sym->u.s.flist; q; q = q->link) {
			appendstr(q->name); appendstr("=");
			tracevalue(field(addrof(e), q->name), lev + 1);
			if (q->link)
				appendstr(",");
		}
		appendstr("}");
		return;
				 }
	case UNION:
		appendstr("("); appendstr(typestring(ty, "")); appendstr("){...}");
		return;
	case ARRAY:
		if (lev && ty->type->size > 0) {
			int i;
			e = pointer(e);
			appendstr("{");
			for (i = 0; i < ty->size/ty->type->size; i++) {
				sTree_t p = (*opnode['+'])(ADD, e, constnode(i, inttype));
				if (isptr(p->type) && isarray(p->type->type))
					p = retype(p, p->type->type);
				else
					p = rvalue(p);
				if (i)
					appendstr(",");
				tracevalue(p, lev + 1);
			}
			appendstr("}");
		} else
			appendstr(typestring(ty, ""));
		return;
	default:
		assert(0);
	}
	if (ty == floattype)
		e = cast(e, doubletype);
	else if ((isint(ty) || isenum(ty)) && ty->size != inttype->size)
		e = cast(e, promote(ty));
	args = tree(ARG + widen(e->type), e->type, e, args);
}
sEvents_t events;

struct entry {
	fApply_t func;
	void * cl;
};

void attach(func, cl, list) fApply_t func; void * cl; sList_t *list; {
	struct entry *p = (struct entry *)alloc(sizeof *p);
	
	p->func = func;
	p->cl = cl;
	*list = append((void *)p, *list);
}
void apply(sList_t list, void * arg1, void *arg2) {
	if (list) {
		sList_t lp = list;
		do {
			struct entry *p = (struct entry *)lp->x;
			(*p->func)(p->cl, arg1, arg2);
			lp = lp->link;
		} while (lp != list);
	}
}
