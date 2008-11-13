dnl  tiger configuration script

AC_INIT(tiger, 1.0)
AC_PREFIX_DEFAULT(/usr/local)
AC_CONFIG_SRCDIR([Makefile.in])


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
