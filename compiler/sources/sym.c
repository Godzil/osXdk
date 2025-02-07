/* C compiler: symbol table management */

#include "c.h"

struct entry {		/* symbol table entry: */
	struct symbol sym;	/* the symbol */
	sList_t refs;		/* list form of sym.uses (omit) */
	struct entry *link;	/* next entry on hash chain */
};

struct table {		/* symbol tables: */
	int level;		/* scope level for this table */
	struct table *previous;	/* table for previous scope */
	sSymbol_t list;		/* list of entries via up fields */
	struct entry *buckets[HASHSIZE];
};
static struct table
	tconstants =   { CONSTANTS },
	texternals =   { GLOBAL },
	tidentifiers = { GLOBAL },
	ttypes =       { GLOBAL };
sTable_t constants	  = &tconstants;	/* constants */
sTable_t externals	  = &texternals;	/* externals */
sTable_t identifiers = &tidentifiers;	/* identifiers */
sTable_t globals	  = &tidentifiers;	/* globals */
sTable_t labels[2];			/* labels */
sTable_t types	  = &ttypes;		/* types */
	
int bnumber;				/* current block number */
int level;				/* current block level */
sList_t symbols;				/* list of all symbols; used only if xref != 0 */

static struct temporary {/* temporaries: */
	sSymbol_t sym;		/* pointer to the symbol */
	struct temporary *link;	/* next available temporary */
} *temps;		/* list of available temporaries */

/* constant - install and return constant v of type ty */
sSymbol_t constant(sType_t ty, uValue_t v)
{
	struct entry *p;
	unsigned h = v.u&(HASHSIZE-1);

	ty = unqual(ty);
	for (p = constants->buckets[h]; p; p = p->link)
		if (eqtype(ty, p->sym.type, 1))
			switch (ty->op) 
			{
			case CHAR:     if (v.uc == p->sym.u.c.v.uc) return &p->sym; break;
			case SHORT:    if (v.ss == p->sym.u.c.v.ss) return &p->sym; break;
			case INT:      if (v.i  == p->sym.u.c.v.i)  return &p->sym; break;
			case UNSIGNED: if (v.u  == p->sym.u.c.v.u)  return &p->sym; break;
			case FLOAT:    if (v.f  == p->sym.u.c.v.f)  return &p->sym; break;
			case DOUBLE:   if (v.d  == p->sym.u.c.v.d)  return &p->sym; break;
			case ARRAY: case FUNCTION:
			case POINTER:  if (v.p  == p->sym.u.c.v.p)  return &p->sym; break;
			default: assert(0);
			}
	p = (struct entry *) alloc(sizeof *p);
	BZERO(&p->sym, struct symbol);
	p->sym.name = vtoa(ty, v);
	p->sym.scope = CONSTANTS;
	p->sym.type = ty;
	p->sym.sclass = STATIC;
	p->sym.u.c.v = v;
	p->link = constants->buckets[h];
	p->sym.up = constants->list;
	constants->list = &p->sym;
	constants->buckets[h] = p;
	p->refs = 0;	/* (omit) */
	defsymbol(&p->sym);
	return &p->sym;
}

/* enterscope - enter a scope */
void enterscope() {
	if (++level >= USHRT_MAX)
		error("compound statements nested too deeply\n");
}

/* exitscope - exit a scope */
void exitscope() {
	rmtypes();
	rmtemps(0, level);
	if (identifiers->level == level) {
		if (Aflag >= 2) {
			int n = 0;
			sSymbol_t p;
			for (p = identifiers->list; p && p->scope == level; p = p->up)
				if (++n > 127) {
					warning("more than 127 identifiers declared in a block\n");
					break;
				}
		}
		if (xref)	/* (omit) */
			setuses(identifiers);	/* (omit) */
		identifiers = identifiers->previous;
	}
	if (types->level == level) {
		if (xref) {	/* (omit) */
			foreach(types, level, fielduses, (void *)0);	/* (omit) */
			setuses(types);	/* (omit) */
		}	/* (omit) */
		types = types->previous;
	}
	assert(level >= GLOBAL);
	--level;
}

/* fielduses - convert use lists for fields in type p */
void fielduses(p, cl) sSymbol_t p; void * cl; {
	if (p->type && isstruct(p->type) && p->u.s.ftab)
		setuses(p->u.s.ftab);
}

/* findlabel - lookup/install label lab in the labels table */
sSymbol_t findlabel(lab) {
	char *label = stringd(lab);
	sSymbol_t p;

	if (p = lookup(label, labels[1]))
		return p;
	p = install(label, &labels[1], 0);
	p->generated = 1;
	p->u.l.label = lab;
	p->u.l.equatedto = p;
	defsymbol(p);
	return p;
}

/* findtype - find type ty in identifiers */
sSymbol_t findtype(ty) sType_t ty; {
	sTable_t tp = identifiers;
	int i;
	struct entry *p;

	assert(tp);
	do
		for (i = 0; i < HASHSIZE; i++)
			for (p = tp->buckets[i]; p; p = p->link)
				if (p->sym.type == ty && p->sym.sclass == TYPEDEF)
					return &p->sym;
	while (tp = tp->previous);
	return 0;
}

/* foreach - call f(p) for each entry p in table tp */
//
void foreach(sTable_t tp, int lev, fApplySymbol_t apply, void *cl)
{
	assert(tp);
	while (tp && tp->level > lev)
		tp = tp->previous;
	if (tp && tp->level == lev) {
		sSymbol_t p;
		sCoordinate_t sav;
		sav = src;
		for (p = tp->list; p && p->scope == lev; p = p->up) {
			src = p->src;
			(*apply)(p, cl);
		}
		src = sav;
	}
}

/* genident - create an identifier with class `sclass', type ty at scope lev */
sSymbol_t genident(int sclass,sType_t ty,int lev)
{
	sSymbol_t p;

	if (lev <= PARAM)
		p = (sSymbol_t) alloc(sizeof *p);
	else
		p = (sSymbol_t) talloc(sizeof *p);
	BZERO(p, struct symbol);
	p->name = stringd(genlabel(1));
	p->scope = lev;
	p->sclass = sclass;
	p->type = ty;
	p->generated = 1;
	if (lev < PARAM)
		defsymbol(p);
	return p;
}

/* genlabel - generate n local labels, return first one */
int genlabel(n) 
{
	static int label = 1;

	label += n;
	return label - n;
}

/* install - install name in table *tp; permanently allocate entry iff perm!=0 */
sSymbol_t install(char *name,sTable_t *tpp,int perm)
{
	struct entry *p;
	unsigned h = (unsigned)name&(HASHSIZE-1);

	if ((tpp == &identifiers || tpp == &types) && (*tpp)->level < level)
		*tpp = table(*tpp, level);
	if (perm)
		p = (struct entry *) alloc(sizeof *p);
	else
		p = (struct entry *) talloc(sizeof *p);
	BZERO(&p->sym, struct symbol);
	p->sym.name = name;
	p->sym.scope = (*tpp)->level;
	p->sym.up = (*tpp)->list;
	(*tpp)->list = &p->sym;
	p->link = (*tpp)->buckets[h];
	(*tpp)->buckets[h] = p;
	p->refs = 0;	/* (omit) */
	return &p->sym;
}

/* intconst - install and return integer constant n */
sSymbol_t intconst(int n) 
{
	uValue_t v;

	v.i = n;
	return constant(inttype, v);
}

/* locus - append (table, cp) to the evolving loci and symbol tables lists */
void locus(sTable_t tp,sCoordinate_t *cp)
{
	extern sList_t loci, tables;

	loci = append((void *)cp, loci);
	tables = append((void *)tp->list, tables);
}

/* lookup - lookup name in table tp, return pointer to entry */
sSymbol_t lookup(char *name,sTable_t tp)
{
	struct entry *p;
	unsigned h = (unsigned)name&(HASHSIZE-1);

	assert(tp);
	do
		for (p = tp->buckets[h]; p; p = p->link)
			if (name == p->sym.name)
				return &p->sym;
	while (tp = tp->previous);
	return 0;
}

/* newconst - install and return constant n with type tc */
sSymbol_t newconst(uValue_t v, int tc) 
{
	static sType_t *btot[] = 
	{ 
		0, 
		&floattype, 
		&doubletype, 
		&chartype,
		&shorttype, 
		&inttype, 
		&unsignedtype, 
		&voidptype 
	};

	assert(tc > 0 && tc < sizeof btot/sizeof btot[0]);
	return constant(*btot[tc], v);
}

/* newtemp - back-end interface to temporary (see below) */
sSymbol_t newtemp(int sclass,int tc) 
{
	sSymbol_t t1;
	static sType_t *btot[] = { 0, &floattype, &doubletype, &chartype,
		&shorttype, &inttype, &unsignedtype, &voidptype };

	assert(tc > 0 && tc < sizeof btot/sizeof btot[0]);
	t1 = temporary(sclass, *btot[tc]);
	t1->scope = LOCAL;
	if (t1->defined == 0) {
		local(t1);
		t1->defined = 1;
	}
	return t1;
}

/* release - release a temporary for re-use */
void release(sSymbol_t t1)
{
	if (t1->ref) 
	{
		struct temporary *p = (struct temporary *) talloc(sizeof *p);
		p->sym = t1;
		p->link = temps;
		temps = p;
		t1->ref = 0;
	}
}

/* rmtemps - remove temporaries at scope `level' or with `sclass' */
void rmtemps(int sclass,int  level) 
{
	struct temporary *p, **q = &temps;

	for (p = *q; p; p = *q)
		if (p->sym->scope == level || p->sym->sclass == sclass)
			*q = p->link;
		else
			q = &p->link;
}

/* setuses - convert p->refs to p->uses for all p at the current level in *tp */
void setuses(sTable_t tp)
{
	if (xref) 
	{
		int i;
		struct entry *p;
		for (i = 0; i < HASHSIZE; i++)
			for (p = tp->buckets[i]; p; p = p->link) 
			{
				if (p->refs)
					p->sym.uses = (sCoordinate_t **)ltoa(p->refs, 0);
				p->refs = 0;
				symbols = append((void *)&p->sym, symbols);
			}
	}
}

/* table - create a new table with predecessor tp, scope lev */
sTable_t table(sTable_t tp,int lev)
{
	int i;
	sTable_t new = (sTable_t)talloc(sizeof *new);

	assert(lev > GLOBAL || lev == LABELS);
	new->previous = tp;
	new->level = lev;
	new->list = tp ? tp->list : 0;	/* (omit) */
	for (i = 0; i < HASHSIZE; i++)
		new->buckets[i] = 0;
	return new;
}

/* temporary - create temporary with class `sclass', type ty */
sSymbol_t temporary(int sclass,sType_t ty)
{
	sSymbol_t t1;
	struct temporary *p, **q = &temps;

	for (p = *q; p; q = &p->link, p = *q)
		if (p->sym->sclass == sclass
		&& p->sym->type->size  == ty->size
		&& p->sym->type->align >= ty->align) {
			*q = p->link;
			p->sym->type = ty;
			return p->sym;
		}
	t1 = genident(sclass, ty, level);
	t1->temporary = 1;
	return t1;
}


/* use - add src to the list of uses for p */
void use(sSymbol_t p, sCoordinate_t src)
{
	if (xref) 
	{
		sCoordinate_t *cp = (sCoordinate_t *)alloc(sizeof *cp);
		*cp = src;
		((struct entry *)p)->refs = append((void *)cp, ((struct entry *)p)->refs);
	}
}
