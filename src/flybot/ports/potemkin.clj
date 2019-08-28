(ns flybot.ports.potemkin
  (:require
    [flybot.ports.potemkin namespaces types collections macros ;; utils
     ]))

(flybot.ports.potemkin.namespaces/import-vars
  flybot.ports.potemkin.namespaces/import-vars) ;; totally meta

(import-vars
  [flybot.ports.potemkin.namespaces

   import-fn
   import-macro
   import-def]

  [flybot.ports.potemkin.macros

   unify-gensyms
   normalize-gensyms
   equivalent?]

  ;; [potemkin.utils

  ;;  condp-case
  ;;  try*
  ;;  fast-bound-fn
  ;;  fast-bound-fn*
  ;;  fast-memoize
  ;;  doit
  ;;  doary]

  [flybot.ports.potemkin.types

   def-abstract-type
   reify+
   defprotocol+
   deftype+
   defrecord+
   definterface+
   extend-protocol+
   ]

  [flybot.ports.potemkin.collections

   reify-map-type
   def-derived-map
   def-map-type])
