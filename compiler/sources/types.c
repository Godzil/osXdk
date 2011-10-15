/* C compiler: type handling */

#include "c.h"

sType_t chartype;			/* char */
sType_t doubletype;		/* double */
sType_t floattype;			/* float */
sType_t inttype;			/* signed int */
sType_t longdouble;		/* long double */
sType_t longtype;			/* long */
sType_t shorttype;			/* signed short int */
sType_t signedchar;		/* signed char */
sType_t unsignedchar;		/* unsigned char */
sType_t unsignedlong;		/* unsigned long int */
sType_t unsignedshort;		/* unsigned short int */
sType_t unsignedtype;		/* unsigned int */
sType_t voidptype;			/* void* */
sType_t voidtype;			/* basic types: void */

static sSymbol_t pointersym;	/* symbol for pointer types */
static struct type {		/* type list entries: */
	struct tynode type;		/* the type */
	struct type *link;		/* next type on the hash chain */
} *typetable[128];		/* current set of types */
static int maxlevel;		/* maximum scope level of entries in typetable */ 

static sField_t check(sType_t, sType_t, sField_t, int);
static sType_t funcl(sType_t, sList_t);
static sField_t isfield(char *, sField_t);
static sType_t tynode(int, sType_t, int, int, void *);

#define VOID_METRICS 0,0,0

/* typeInit - initialize basic types */
void typeInit() {
#define xx(v,name,op,metrics) { sSymbol_t p = install(string(name), &types, 1); \
	v = p->type = tynode(op, 0, metrics+1?(void *)p:0); p->addressed = (metrics); \
	assert(v->align == 0 || v->size%v->align == 0); }
	xx(chartype,	  "char",		CHAR,	  CHAR_METRICS);
	xx(doubletype,	  "double",		DOUBLE,	  DOUBLE_METRICS);
	xx(floattype,	  "float",		FLOAT,	  FLOAT_METRICS);
	xx(inttype,	  "int",		INT,	  INT_METRICS);
	xx(longdouble,	  "long double",	DOUBLE,	  DOUBLE_METRICS);
	xx(longtype,	  "long int",		INT,	  INT_METRICS);
	xx(shorttype,	  "short",		SHORT,	  SHORT_METRICS);
	xx(signedchar,	  "signed char",	CHAR,	  CHAR_METRICS);
	xx(unsignedchar,  "unsigned char",	CHAR,	  CHAR_METRICS);
	xx(unsignedlong,  "unsigned long int",	UNSIGNED, INT_METRICS);
	xx(unsignedshort, "unsigned short int",	SHORT,	  SHORT_METRICS);
	xx(unsignedtype,  "unsigned int",	UNSIGNED, INT_METRICS);
	xx(voidtype,	  "void",		VOID,	  VOID_METRICS);
#undef xx

#define xx(v,name,adr) v = install(string(name), &types, 1); v->addressed = (adr);
	xx(pointersym,	"T*",	 POINTER_METRICS);
#undef xx

	voidptype = ptr(voidtype);
	assert(voidptype->align > 0 && voidptype->size%voidptype->align == 0);
	assert(unsignedtype->size >= voidptype->size);
}

/* array - construct the type `array 0..n-1 of ty' with alignment a or ty's */
sType_t array(ty, n, a) sType_t ty; {
	if (ty && isfunc(ty)) {
		error("illegal type `array of %t'\n", ty);
		return array(inttype, n, 0);
	}
	if (a == 0)
		a = ty->align;
	if (level > GLOBAL && isarray(ty) && ty->size == 0)
		error("missing array size\n");
	if (ty->size == 0) {
		if (unqual(ty) == voidtype)
			error("illegal type `array of %t'\n", ty);
		else if (Aflag >= 2)
			warning("declaring type `array of %t' is undefined\n", ty);
	} else if (n > INT_MAX/ty->size) {
		error("size of `array of %t' exceeds %d bytes\n", ty, INT_MAX);
		n = 1;
	}
	return tynode(ARRAY, ty, n*ty->size, a, NULL);
}

/* atop - convert ty from `array of ty' to `pointer to ty' */
sType_t atop(ty) sType_t ty; {
	if (isarray(ty))
		return ptr(ty->type);
	error("type error: %s\n", "array expected");
	return ptr(ty);
}

/* check - check ty for ambiguous inherited fields, return augmented field set */
static sField_t check(ty, top, inherited, off) sType_t ty, top; sField_t inherited; {
	sField_t p;

	for (p = ty->u.sym->u.s.flist; p; p = p->link)
		if (p->name && isfield(p->name, inherited))
			error("ambiguous field `%s' of `%t' from `%t'\n", p->name, top, ty);
		else if (p->name && !isfield(p->name, top->u.sym->u.s.flist)) {
			sField_t new = (sField_t) talloc(sizeof *new);
			*new = *p;
			new->offset = off + p->offset;
			new->link = inherited;
			inherited = new;
		}
	for (p = ty->u.sym->u.s.flist; p; p = p->link)
		if (p->name == 0)
			inherited = check(p->type, top, inherited,
				off + p->offset);
	return inherited;
}

/* checkfields - check for ambiguous inherited fields in struct/union ty */
void checkfields(ty) sType_t ty; {
	sField_t p, inherited = 0;

	for (p = ty->u.sym->u.s.flist; p; p = p->link)
		if (p->name == 0)
			inherited = check(p->type, ty, inherited, p->offset);
}

/* composite - return the composite type of ty1 & ty2, or 0 if ty1 & ty2 are incompatible */
sType_t composite(ty1, ty2) sType_t ty1, ty2; {
	if (ty1 == ty2)
		return ty1;
	if (ty1->op != ty2->op)
		return 0;
	switch (ty1->op) {
	case CONST+VOLATILE:
		return qual(CONST, qual(VOLATILE, composite(ty1->type, ty2->type)));
	case CONST: case VOLATILE:
		return qual(ty1->op, composite(ty1->type, ty2->type));
	case POINTER:
		return ptr(composite(ty1->type, ty2->type));
	case ARRAY: {
		sType_t ty;
		if (ty = composite(ty1->type, ty2->type)) {
			if (ty1->size && ty1->type->size && ty2->size == 0)
				return array(ty, ty1->size/ty1->type->size, ty1->align);
			if (ty2->size && ty2->type->size && ty1->size == 0)
				return array(ty, ty2->size/ty2->type->size, ty2->align);
			return array(ty, 0, 0);
		}
		break;
		}
	case FUNCTION: {
		sType_t ty;
		sList_t list = 0;
		if (ty = composite(ty1->type, ty2->type)) {
			sType_t *p1, *p2, proto = 0;
			if (ty1->u.proto && ty2->u.proto == 0)
				return func(ty, ty1->u.proto);
			if (ty2->u.proto && ty1->u.proto == 0)
				return func(ty, ty2->u.proto);
			for (p1 = ty1->u.proto, p2 = ty2->u.proto; *p1 && *p2; p1++, p2++) {
				sType_t ty;
				if ((ty = composite(unqual(*p1), unqual(*p2))) == 0)
					return 0;
				if (isconst(*p1) || isconst(*p2))
					ty = qual(CONST, ty);
				if (isvolatile(*p1) || isvolatile(*p2))
					ty = qual(VOLATILE, ty);
				list = append(ty, list);
			}
			if (*p1 || *p2)
				return 0;
			return funcl(ty, list);
		}
		break;
		}
	case CHAR:   case SHORT: case INT:     case DOUBLE:
	case VOID:   case FLOAT: case UNSIGNED:
	case STRUCT: case UNION: case ENUM:
		break;
	default:
		assert(0);
	}
	return 0;
}

/* deftype - define name to be equivalent to type ty */
sSymbol_t deftype(char *name, sType_t ty, sCoordinate_t *pos)
{
	sSymbol_t p = lookup(name, identifiers);

	if (p && p->scope == level)
		error("redeclaration of `%s'\n", name);
	p = install(name, &identifiers, level < LOCAL);
	p->type = ty;
	p->sclass = TYPEDEF;
	p->src = *pos;
	return p;
}

/* deref - dereference ty, type *ty */
sType_t deref(ty) sType_t ty; {
	if (isptr(ty))
		ty = ty->type;
	else
		error("type error: %s\n", "pointer expected");
	return isenum(ty) ? unqual(ty)->type : ty;
}

/* eqtype - is ty1==ty2?  handles arrays & functions; return ret if ty1==ty2, but one is incomplete */
int eqtype(ty1, ty2, ret) sType_t ty1, ty2; {
	if (ty1 == ty2)
		return 1;
	if (ty1->op != ty2->op)
		return 0;
	switch (ty1->op) {
	case CONST: case VOLATILE: case CONST+VOLATILE: case POINTER:
		return eqtype(ty1->type, ty2->type, ret);
	case ARRAY:
		if (eqtype(ty1->type, ty2->type, ret)) {
			if (ty1->size == ty2->size)
				return 1;
			if (ty1->size == 0 && ty2->size >  0
			||  ty1->size >  0 && ty2->size == 0)
				return ret;
		}
		break;
	case FUNCTION:
		if (eqtype(ty1->type, ty2->type, ret)) {
			sType_t *p1 = ty1->u.proto, *p2 = ty2->u.proto;
			if (p1 == p2)
				return 1;
			if (p1 == 0 || p2 == 0) {
				if (p1 == 0) {
					ty1 = ty2;
					p1 = p2;
				}
				for ( ; *p1; p1++) {
					sType_t ty = unqual(*p1);
					if (promote(ty) != ty || ty == floattype
					|| ty == voidtype && p1 != ty1->u.proto)
						return 0;
				}
				return 1;
			}
			for ( ; *p1 && *p2; p1++, p2++)
				if (eqtype(unqual(*p1), unqual(*p2), ret) == 0)
/*				if (eqtype(*p1, *p2, ret) == 0) */
					return 0;
			if (*p1 == 0 && *p2 == 0)
				return 1;
		}
		break;
	case CHAR:   case SHORT: case INT:     case DOUBLE:
	case VOID:   case FLOAT: case UNSIGNED:
	case STRUCT: case UNION: case ENUM:
		break;
	default:
		assert(0);
	}
	return 0;
}

/* extends - if ty extends fty, return a pointer to field structure */
sField_t extends(ty, fty) sType_t ty, fty; {
	sField_t p, q;

	for (p = unqual(ty)->u.sym->u.s.flist; p; p = p->link)
		if (p->name == 0 && unqual(p->type) == unqual(fty))
			return p;
		else if (p->name == 0 && (q = extends(p->type, fty))) {
			static struct field f;
			f = *q;
			f.offset = p->offset + q->offset;
			return &f;
		}
	return 0;
}

/* fieldlist - construct a flat list of fields in type ty */
sField_t fieldlist(ty) sType_t ty; {
	sField_t p, q, t, inherited = 0, *r;

	ty = unqual(ty);
	for (p = ty->u.sym->u.s.flist; p; p = p->link)
		if (p->name == 0)
			inherited = check(p->type, ty, inherited, p->offset);
	if (inherited == 0)
		return ty->u.sym->u.s.flist;
	for (q = 0, p = inherited; p; q = p, p = t) {
		t = p->link;
		p->link = q;
	}
	for (r = &inherited, p = ty->u.sym->u.s.flist; p && q; )
		if (p->name == 0)
			p = p->link;
		else if (p->offset <= q->offset) {
			*r = (sField_t) talloc(sizeof **r);
			**r = *p;
			r = &(*r)->link;
			p = p->link;
		} else {
			*r = q;
			r = &q->link;
			q = q->link;
		}
	for ( ; p; p = p->link)
		if (p->name) {
			*r = (sField_t) talloc(sizeof **r);
			**r = *p;
			r = &(*r)->link;
		}
	*r = q;
	return inherited;
}

/* fieldref - find field name of type ty, return entry */
sField_t fieldref(name, ty) char *name; sType_t ty; {
	sField_t p;

	if (p = isfield(name, unqual(ty)->u.sym->u.s.flist)) {
		if (xref) {
			sSymbol_t q;
			assert(unqual(ty)->u.sym->u.s.ftab);
			q = lookup(name, unqual(ty)->u.sym->u.s.ftab);
			assert(q);
			use(q, src);
		}
		return p;
	}
	for (p = unqual(ty)->u.sym->u.s.flist; p; p = p->link) {
		sField_t q;
		if (p->name == 0 && (q = fieldref(name, p->type))) {
			static struct field f;
			f = *q;
			f.offset = p->offset + q->offset;
			return &f;
		}
	}
	return 0;
}

/* freturn - for `function returning ty', return ty */
sType_t freturn(sType_t ty)
{
	if (isfunc(ty))
		return ty->type;
	error("type error: %s\n", "function expected");
	return inttype;
}

/* func - construct the type `function (proto) returning ty' */
sType_t func(sType_t ty, sType_t *proto)
{
	if (ty && (isarray(ty) || isfunc(ty)))
		error("illegal return type `%t'\n", ty);
	return tynode(FUNCTION, ty, 0, 0, (void *)proto);
}

/* funcl - construct the type `function (list) returning ty' */
static sType_t funcl(sType_t ty, sList_t list)
{
	return func(ty, (sType_t *)ltoa(list, (void * *)alloc((length(list) + 1)*sizeof (sType_t))));
}

/* hasproto - true iff ty has no function types or they all have prototypes */
int hasproto(sType_t ty)
{
	if (Aflag < 1 || ty == 0)
		return 1;
	switch (ty->op) {
	case CONST: case VOLATILE: case CONST+VOLATILE: case POINTER:
	case ARRAY:
		return hasproto(ty->type);
	case FUNCTION:
		return hasproto(ty->type) && ty->u.proto;
	case STRUCT: case UNION:
	case CHAR:   case SHORT: case INT:  case DOUBLE:
	case VOID:   case FLOAT: case ENUM: case UNSIGNED:
		return 1;
	default:
		assert(0);
	}
	return 0;
}

/* isfield - if name is a field in flist, return pointer to the field structure */
static sField_t isfield(name, flist) char *name; sField_t flist; {
	for ( ; flist; flist = flist->link)
		if (flist->name == name)
			break;
	return flist;
}

/* newfield - install a new field in ty with type fty */
sField_t newfield(char *name, sType_t ty, sType_t fty)
{
	sField_t p, *q = &ty->u.sym->u.s.flist;

	if (name == 0)
		name = stringd(genlabel(1));
	for (p = *q; p; q = &p->link, p = *q)
		if (p->name == name)
			error("duplicate field name `%s' in `%t'\n", name, ty);
	*q = p = (sField_t)alloc(sizeof *p);
	BZERO(p, struct field);
	p->name = name;
	p->type = fty;
	if (xref) {
		if (ty->u.sym->u.s.ftab == 0)
			ty->u.sym->u.s.ftab = table(0, level);
		install(name, &ty->u.sym->u.s.ftab, 1)->src = src;
	}
	return p;
}

/* newstruct - install a new structure/union/enum depending on op */
sType_t newstruct(int op, char *tag)
{
	sSymbol_t p;

	if (!tag || *tag == '\0')  /* anonymous structure/union/enum */
		tag = stringd(genlabel(1));
	if ((p = lookup(tag, types)) && (p->scope == level
	|| p->scope == PARAM && level == PARAM+1)) {
		if (p->type->op == op && !p->defined)
			return p->type;
		error("redeclaration of `%s'\n", tag);
	}
	p = install(tag, &types, 1);
	p->type = tynode(op, 0, 0, 0, p);
	if (p->scope > maxlevel)
		maxlevel = p->scope;
	p->src = src;
	return p->type;
}

/* outtype - output type ty */
void outtype(sType_t ty)
{
	switch (ty->op) {
	case CONST+VOLATILE:
		print("%k %k %t", CONST, VOLATILE, ty->type);
		break;
	case CONST: case VOLATILE:
		print("%k %t", ty->op, ty->type);
		break;
	case STRUCT: case UNION: case ENUM:
		assert(ty->u.sym);
		if (ty->size == 0)
			print("incomplete ");
		assert(ty->u.sym->name);
		if (*ty->u.sym->name >= '1' && *ty->u.sym->name <= '9') {
			sSymbol_t p = findtype(ty);
			if (p == 0)
				print("%k defined at %w", ty->op, &ty->u.sym->src);
			else
				print(p->name);
		} else {
			print("%k %s", ty->op, ty->u.sym->name);
			if (ty->size == 0)
				print(" defined at %w", &ty->u.sym->src);
		}
		break;
	case VOID: case FLOAT: case DOUBLE:
	case CHAR: case SHORT: case INT: case UNSIGNED:
		print(ty->u.sym->name);
		break;
	case POINTER:
		print("pointer to %t", ty->type);
		break;
	case FUNCTION:
		print("%t function", ty->type);
		if (ty->u.proto) {
			sType_t *p = ty->u.proto;
			print("(%t", *p);
			while (*++p)
				if (*p == voidtype)
					print(",...");
				else
					print(",%t", *p);
			print(")");
		}
		break;
	case ARRAY:
		if (ty->size > 0 && ty->type && ty->type->size > 0) {
			print("array %d", ty->size/ty->type->size);
			while (ty->type && isarray(ty->type) && ty->type->type->size > 0) {
				ty = ty->type;
				print(",%d", ty->size/ty->type->size);
			}
		} else
			print("incomplete array");
		if (ty->type)
			print(" of %t", ty->type);
		break;
	default:
		assert(0);
	}
}

/* printdecl - output a C declaration for symbol p of type ty */
void printdecl(sSymbol_t p, sType_t ty) {
	switch (p->sclass) {
	case AUTO:
		fprint(2, "%s;\n", typestring(ty, p->name));
		break;
	case STATIC: case EXTERN:
		fprint(2, "%k %s;\n", p->sclass, typestring(ty, p->name));
	case TYPEDEF: case ENUM:
		break;
	default:
		assert(0);
	}
}

/* printproto - output a prototype declaration for function p */
void printproto(sSymbol_t p, sSymbol_t callee[])
{
	if (p->type->u.proto)
		printdecl(p, p->type);
	else {
		int i;
		sList_t list = 0;
		if (callee[0] == 0)
			list = append(voidtype, list);
		else
			for (i = 0; callee[i]; i++)
				list = append(callee[i]->type, list);
		printdecl(p, funcl(freturn(p->type), list));
	}
}

/* printtype - print details of type ty on fd */
void printtype(sType_t ty, int fd)
{
	switch (ty->op) {
	case STRUCT: case UNION: {
		sField_t p;
		fprint(fd, "%k %s size=%d {\n", ty->op, ty->u.sym->name, ty->size);
		for (p = ty->u.sym->u.s.flist; p; p = p->link) {
			fprint(fd, "field %s: offset=%d", p->name, p->offset);
			if (p->to)
				fprint(fd, " bits=%d..%d", p->from, p->to);
			fprint(fd, " type=%t", p->type);
		}
		fprint(fd, "}\n");
		break;
		}
	case ENUM: {
		int i;
		sSymbol_t p;
		fprint(fd, "enum %s {", ty->u.sym->name);
		for (i = 0; p = ty->u.sym->u.idlist[i]; i++) {
			if (i > 0)
				fprint(fd, ",");
			fprint(fd, "%s=%d", p->name, p->u.value);
		}
		fprint(fd, "}\n");
		break;
		}
	default:
		fprint(fd, "%t\n", ty);
	}
}

/* ptr - construct the type `pointer to ty' */
sType_t ptr(ty) sType_t ty; {
	return tynode(POINTER, ty, POINTER_METRICS+1?(void *)pointersym:0);
}

/* qual - construct the type `op ty' where op is CONST or VOLATILE */
sType_t qual(op, ty) sType_t ty; {
	if (isarray(ty))
		ty = tynode(ARRAY, qual(op, ty->type), ty->size,
			ty->align, 0);
	else if (isfunc(ty))
		warning("qualified function type ignored\n");
	else if (isconst(ty) && op == CONST || isvolatile(ty) && op == VOLATILE)
		error("illegal type `%k %t'\n", op, ty);
	else {
		int i;
		struct type *tn;
		if (isqual(ty)) {
			op += ty->op;
			ty = ty->type;
		}
		for (i = 0; i < sizeof typetable/sizeof typetable[0]; i++)
			for (tn = typetable[i]; tn; tn = tn->link)
				if (tn->type.op == op && tn->type.type == ty) {
					tn->type.size = ty->size;
					tn->type.align = ty->align;
					return &tn->type;
				}
		return tynode(op, ty, ty->size, ty->align, 0);
	}
	return ty;
}

/* rmtypes - remove type nodes at the current scope level */
void rmtypes() {
	if (maxlevel >= level) {
		int i;
		maxlevel = 0;
		for (i = 0; i < sizeof typetable/sizeof typetable[0]; i++) {
			struct type *tn, **tq = &typetable[i];
			for (tn = *tq; tn; tn = *tq)
				if (tn->type.op == FUNCTION)
					tq = &tn->link;
				else if (tn->type.u.sym && tn->type.u.sym->scope == level)
					*tq = tn->link;
				else {
					if (tn->type.u.sym && tn->type.u.sym->scope > maxlevel)
						maxlevel = tn->type.u.sym->scope;
					tq = &tn->link;
				}
		}
	}
}

/* ttob - map arbitrary type ty to integer basic type */
int ttob(ty) sType_t ty; {
	switch (ty->op) {
	case CONST: case VOLATILE: case CONST+VOLATILE:
		return ttob(ty->type);
	case VOID: case CHAR: case INT: case SHORT:
	case UNSIGNED: case FLOAT: case DOUBLE:
		return ty->op;
	case POINTER: case FUNCTION:
		return POINTER;
	case ARRAY: case STRUCT: case UNION:
		return STRUCT;
	case ENUM:
		return INT;
	default:
		assert(0);
	}
	return I;
}

/* tynode - allocate and initialize a type node */
static sType_t tynode(int op, sType_t type, int size, int align, void *ptr)
{
	int i = (opindex(op)^((unsigned)type>>2))&(sizeof typetable/sizeof typetable[0]-1);
	struct type *tn;

	if (op != ARRAY || size > 0)
		for (tn = typetable[i]; tn; tn = tn->link)
			if (tn->type.op   == op   && tn->type.type  == type
			&&  tn->type.size == size && tn->type.align == align
			&&  tn->type.u.ptr == ptr)
				return &tn->type;
	tn = (struct type *) alloc(sizeof *tn);
	tn->type.op = op;
	tn->type.type = type;
	tn->type.size = size;
	tn->type.align = align;
	tn->type.u.ptr = ptr;
	BZERO((&tn->type.x), sXtype_t);
	tn->link = typetable[i];
	typetable[i] = tn;
	return &tn->type;
}

/* typestring - return ty as C declaration for str, which may be "" */
char *typestring(ty, str) sType_t ty; char *str; {
	for ( ; ty; ty = ty->type) {
		sSymbol_t p;
		switch (ty->op) {
		case CONST+VOLATILE:
			if (isptr(ty->type))
				str = stringf("%k %k %s", CONST, VOLATILE, str);
			else
				return stringf("%k %k %s", CONST, VOLATILE, typestring(ty->type, str));
			break;
		case CONST: case VOLATILE:
			if (isptr(ty->type))
				str = stringf("%k %s", ty->op, str);
			else
				return stringf("%k %s", ty->op, typestring(ty->type, str));
			break;
		case STRUCT: case UNION: case ENUM:
			assert(ty->u.sym);
			if (p = findtype(ty))
				return *str ? stringf("%s %s", p->name, str) : p->name;
			if (*ty->u.sym->name >= '1' && *ty->u.sym->name <= '9')
				warning("unnamed %k in prototype\n", ty->op);
			if (*str)
				return stringf("%k %s %s", ty->op, ty->u.sym->name, str);
			else
				return stringf("%k %s", ty->op, ty->u.sym->name);
		case VOID: case FLOAT: case DOUBLE:
		case CHAR: case SHORT: case INT: case UNSIGNED:
			return *str ? stringf("%s %s", ty->u.sym->name, str) : ty->u.sym->name;
		case POINTER:
			if (unqual(ty->type)->op != CHAR && (p = findtype(ty)))
				return *str ? stringf("%s %s", p->name, str) : p->name;
			str = stringf(isarray(ty->type) || isfunc(ty->type) ? "(*%s)" : "*%s", str);
			break;
		case FUNCTION:
			if (p = findtype(ty))
				return *str ? stringf("%s %s", p->name, str) : p->name;
			if (ty->u.proto == 0)
				str = stringf("%s()", str);
			else {
				sType_t *proto = ty->u.proto;
				str = stringf("%s(%s", str, typestring(*proto, ""));
				while (*++proto)
					if (*proto == voidtype)
						str = stringf("%s, ...", str);
					else
						str = stringf("%s, %s", str, typestring(*proto, ""));
				str = stringf("%s)", str);
			}
			break;
		case ARRAY:
			if (p = findtype(ty))
				return *str ? stringf("%s %s", p->name, str) : p->name;
			if (ty->type && ty->type->size > 0)
				str = stringf("%s[%d]", str, ty->size/ty->type->size);
			else
				str = stringf("%s[]", str);
			break;
		default:
			assert(0);
		}
	}
	assert(0);
	return 0;
}

/* variadic - is function type ty variadic? */
int variadic(ty) sType_t ty; {
	if (isfunc(ty) && ty->u.proto) {
		int i;
		for (i = 0; ty->u.proto[i]; i++)
			;
		return i > 1 && ty->u.proto[i-1] == voidtype;
	}
	return 0;
}
