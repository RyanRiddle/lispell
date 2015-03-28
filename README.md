# lispell
Spell check in Common Lisp

Usage

   (read-dict "/usr/share/dict/american-english")
   
   (check "butter")
   ; returns T
   
   (check "chese")
   ; returns NIL
   
   (suggest "chese")
   ; returns ("cheese" "chest" "these" "chess" "chose" "chase")
