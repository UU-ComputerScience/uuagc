dnl  tiger configuration script

AC_INIT(tiger, 1.0)
AC_PREFIX_DEFAULT(/usr/local)
AC_CONFIG_SRCDIR([Makefile.in])

# other programs
AC_PATH_PROG(MV,mv)
AC_PATH_PROG(CP,cp)
AC_PATH_PROG(RM,rm)
AC_PATH_PROG(MKDIR,mkdir)
AC_PATH_PROG(TOUCH,touch)
AC_PATH_PROG(DIFF,diff)
AC_PATH_PROG(FIND,find)
AC_PATH_PROG(SORT,sort)
AC_PATH_PROG(UNIQ,uniq)
AC_PATH_PROG(AR,ar)
AC_PATH_PROG(RANLIB,ranlib)
AC_PATH_PROG(LD,ld)
AC_PATH_PROG(GREP,grep)
AC_PATH_PROG(CHMOD,chmod)
AC_PATH_PROG(LHS2TEX,lhs2TeX)


dnl find AG compiler

AC_PATH_PROG(UUAGC,uuagc)
AC_ARG_WITH(uuagc,
[ --with-uuagc=<UUAG compiler>
      Use the specified UUAG compiler
]
, [WithUUAGC="$withval"]
, [WithUUAGC="$UUAGC"]
)
UUAGC="$WithUUAGC"
AC_SUBST(UUAGC)
if test "$UUAGC" = ""; then
  AC_MSG_ERROR([UUAG compiler not found])
fi


dnl find GHC compiler

AC_PATH_PROG(GHC,ghc)
AC_ARG_WITH(ghc,
[ --with-ghc=<GHC compiler>
      Use the specified GHC compiler
]
, [WithGHC="$withval"]
, [WithGHC="$GHC"]
)
GHC="$WithGHC"
AC_SUBST(GHC)
if test "$GHC" = ""; then
  AC_MSG_ERROR([GHC compiler not found])
fi


dnl find Alex scanner generator

AC_PATH_PROG(ALEX,alex)
AC_ARG_WITH(alex,
[ --with-alex=<Alex scanner generator>
      Use the specified Alex scanner generator
]
, [WithALEX="$withval"]
, [WithALEX="$ALEX"]
)
ALEX="$WithALEX"
AC_SUBST(ALEX)
if test "$ALEX" = ""; then
  AC_MSG_ERROR([Alex scanner generator not found])
fi


AC_OUTPUT(Makefile)
