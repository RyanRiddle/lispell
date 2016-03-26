# lispell
Fast spell check in Common Lisp.

##Usage

The spellchecker needs a dictionary.  Load a list of words each on a newline like this.

   `(read-dict "/usr/share/dict/american-english")`
   
Check words like this.  For example, butter and cheese.
   
   `(check "butter")`
   `; returns T`
   
   `(check "chese")`
   `; returns NIL`
   
If you want suggestions for misspelled words, do this.

   `(build-misspellings)`
   `; returns NIL`
   
Note: This operation may take some time.  For a dictionary of ~100,000 words it took about 15 seconds in SBCL and about 2 minutes in CLISP.
   
Get suggestions like this.
   
   `(suggest "chese")`
   `; returns ("cheese" "chest" "these" "chess" "chose" "chase")`
   
If you pass a correctly spelled word to suggest, it returns NIL.

   `(suggest "cheese")`
   ; returns NIL

##Implementation Details

This implementation is based on the Faroo Symmetric Delete Spelling Correction algorithm.  Read about it in detail here: http://blog.faroo.com/2012/06/07/improved-edit-distance-based-spelling-correction/

The Symmetric Delete algorithm sacrifices space for speed.  It computes edit-distance <= 2 terms for each word in your dictionary and adds them to a hash table.  The keys are the edit-distance <= 2 terms and the values are the correct spellings.

When you ask for suggestions for a misspelling, we compute the edit-distance <= 2 terms for the misspelling and look for each term in our hash table of misspellings.  The results are returned.
The advantage of this algorithm is that it returns suggestions in O(1) time.

##TODO
Use hunspell-style dictionaries with an affix file and a dictionary file.

##Compatibility
Tested with SBCL v1.1.14.debian and CLISP v2.49

##License
Use it.  I don't care how.
