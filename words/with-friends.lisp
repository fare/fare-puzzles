#| ;-*- Lisp -*-
# I never actually wrote a main function and an interface.
# See below on how to use this program from the Lisp REPL
#!/usr/bin/env cl-launch -sm fare-puzzles/words/with-friends --

;; The most elaborate interface I ever used was the play-game macro.
;; example:
 (asdf:make :fare-puzzles/words/with-friends)
 (fare-puzzle/words/with-friends:play-game
   (7 7 :d "hello")
   "worldab"))

When my friend Cara invited me to play "words with friends",
she told me that I should only use my brain, or else it would be cheating.
After frustratingly spending too much time reordering letters in my mind
checking dictionaries for short words acceptable in scrabble,
and counting how much a word would score,
I finally wrote my own program to do that grunt work for me.
Since the program was the fruit of my brain, and wasn't using somebody else's,
I considered that wasn't cheating.
Still, when I admitted to having written it,
all my friends stopped playing with me. Oh well.

This little piece of software will help you play at "Words with friends",
or alternatively at Scrabble. What it does do is implement a simple algorithm
to list all available options for the next move,
sorted by increasing score value.

What is doesn't do is provide any fancy heuristic to take into account
what you or your opponent might play afterwards.
It doesn't take into account the difficulty of keeping some letters
rather than other letters.
It won't give you advice on which higher-scoring words to avoid
because it then enables your opponent to use
a 3x square or pair of 2x squares, or
other high value combination with valuable letters,
increasing his expected score beyond the extra points
from a lower-scoring word at this move of yours.
It won't try at all to guess what letters
your opponent is likely to have or not have
based on what he previously played, and/or on
what letters are left to be played.
In particular it doesn't at all try to optimize end-games
where all the letters possessed by the opponent are known,
or near end-games where only a few possibilities of end-games remain.
I didn't even start to think about implementing such sophisticated heuristics.
My simple algorithm to list all possible combinations for the current move
was more than enough to take a serious lead over my friends
who hadn't written theirs.

I guide to Words With Friends can be found here:
   http://osxreality.com/2010/01/01/beginners-guide-to-words-with-friends-2/

|#

(uiop:define-package :fare-puzzles/words/with-friends
  (:use :cl :fare-utils :uiop :inferior-shell)
  (:export #:play-game))

(in-package :fare-puzzles/words/with-friends)

(declaim (optimize (speed 1) (debug 3) (safety 3))
         #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

(defparameter *dictionary-url*
  "http://dotnetperls-controls.googlecode.com/files/enable1.txt"
  "A dictionary for Scrabble, more or less what the official WWF server uses")

(defparameter *dictionary-path*
  (asdf:system-relative-pathname :fare-puzzles "data/enable1.txt"))

(defparameter *letter-values*
  #(0 1 4 4 2 1 4 3 3 1 10 5 2 4 2 1 4 10 1 1 1 2 5 4 8 3 10)) ; joker a-z

(defparameter *letter-occurrences*
  #(2 9 2 2 5 13 2 3 4 8 1 1 4 2 5 8 2 1 6 5 7 4 2 2 1 2 1)) ; joker a-z

(defparameter *two-letter-words*
 '(AA AB AD AE AG AH AI AL AM AN AR AS AT
   AW AX AY BA BE BI BO BY DA DE DI DO ED
   EF EH EL EM EN ER ES ET EX FA FE FI GO
   GI HA HE HI HM HO ID IF IN IS IT JO KA
   KI LA LI LO MA ME MI MM MO MU MY NA NE
   NO NU OD OE OF OH OI OM ON OP OR OS OW
   OX OY PA PE PI QI RE SH SI SO TA TI TO
   UH UM UN UP US UT WE WO XI XU YA YE YO
   ZA))

(defparameter *three-letter-words*
 '(AAH AAL AAS ABA ABO ABS ABY ACE ACT ADD ADO ADS ADZ
   AFF AFT AGA AGE AGO AHA AHI AID AIL AIM AIN AIR AIS
   AIT ALA ALB ALE ALL ALP ALS ALT AMA AMI AMP AMU ANA
   AND ANE ANI ANT ANY APE APT ARB ARC ARE ARF ARK ARM
   ARS ART ASH ASK ASP ASS ATE ATT AUK AVA AVE AVO AWA
   AWE AWL AWN AXE AYE AYS AZO
   BAA BAD BAG BAH BAL BAM BAN BAP BAR BAS BAT BAY BED
   BEE BEG BEL BEN BET BEY BIB BID BIG BIN BIO BIS BIT
   BIZ BOA BOB BOD BOG BOO BOP BOS BOT BOW BOX BOY BRA
   BRO BRR BUB BUD BUG BUM BUN BUR BUS BUT BUY BYE BYS
   CAB CAD CAM CAN CAP CAR CAT CAW CAY CEE CEL CEP CHI
   CIS COB COD COG COL CON COO COP COR COS COT COW COX
   COY COZ CRY CUB CUD CUE CUM CUP CUR CUT CWM
   DAB DAD DAG DAH DAK DAL DAM DAP DAW DAY DEB DEE DEL
   DEN DEV DEW DEX DEY DIB DID DIE DIG DIM DIN DIP DIS
   DIT DOC DOE DOG DOL DOM DON DOR DOS DOT DOW DRY DUB
   DUD DUE DUG DUI DUN DUO DUP DYE
   EAR EAT EAU EBB ECU EDH EEL EFF EFS EFT EGG EGO EKE
   ELD ELF ELK ELL ELM ELS EME EMF EMS EMU END ENG ENS
   EON ERA ERE ERG ERN ERR ERS ESS ETA ETH EVE EWE EYE
   FAD FAN FAR FAS FAT FAX FAY FED FEE FEH FEM FEN FER
   FET FEU FEW FEY FEZ FIB FID FIE FIG FIL FIN FIR FIT
   FIX FIZ FLU FLY FOB FOE FOG FOH FON FOP FOR FOU FOX
   FOY FRO FRY FUB FUD FUG FUN FUR
   GAB GAD GAE GAG GAL GAM GAN GAP GAR GAS GAT GAY GED
   GEE GEL GEM GEN GET GEY GHI GIB GID GIE GIG GIN GIP
   GIT GNU GOA GOB GOD GOO GOR GOT GOX GOY GUL GUM GUN
   GUT GUV GUY GYM GYP
   HAD HAE HAG HAH HAJ HAM HAO HAP HAS HAT HAW HAY HEH
   HEM HEN HEP HER HES HET HEW HEX HEY HIC HID HIE HIM
   HIN HIP HIS HIT HMM HOB HOD HOE HOG HON HOP HOT HOW
   HOY HUB HUE HUG HUH HUM HUN HUP HUT HYP
   ICE ICH ICK ICY IDS IFF IFS ILK ILL IMP INK INN INS
   ION IRE IRK ISM ITS IVY
   JAB JAG JAM JAR JAW JAY JEE JET JEU JIB JIG JIN JOB
   JOE JOG JOT JOW JOY JUG JUN JUS JUT
   KAB KAE KAF KAS KAT KAY KEA KEF KEG KEN KEP KEX KEY
   KHI KID KIF KIN KIP KIR KIT KOA KOB KOI KOP KOR KOS
   KUE
   LAB LAC LAD LAG LAM LAP LAR LAS LAT LAV LAW LAX LAY
   LEA LED LEE LEG LEI LEK LET LEU LEV LEX LEY LEZ LIB
   LID LIE LIN LIP LIS LIT LOB LOG LOO LOP LOT LOW LOX
   LUG LUM LUV LUX LYE
   MAC MAD MAE MAG MAN MAP MAR MAS MAT MAW MAX MAY MED
   MEL MEM MEN MET MEW MHO MIB MID MIG MIL MIM MIR MIS
   MIX MOA MOB MOC MOD MOG MOL MOM MON MOO MOP MOR MOS
   MOT MOW MUD MUG MUM MUN MUS MUT
   NAB NAE NAG NAH NAM NAN NAP NAW NAY NEB NEE NET NEW
   NIB NIL NIM NIP NIT NIX NOB NOD NOG NOH NOM NOO NOR
   NOS NOT NOW NTH NUB NUN NUS NUT
   OAF OAK OAR OAT OBE OBI OCA ODD ODE ODS OES OFF OFT
   OHM OHO OHS OIL OKA OKE OLD OLE OMS ONE ONS OOH OOT
   OPE OPS OPT ORA ORB ORC ORE ORS ORT OSE OUD OUR OUT
   OVA OWE OWL OWN OXO OXY
   PAC PAD PAH PAL PAM PAN PAP PAR PAS PAT PAW PAX PAY
   PEA PEC PED PEE PEG PEH PEN PEP PER PES PET PEW PHI
   PHT PIA PIC PIE PIG PIN PIP PIS PIT PIU PIX PLY POD
   POH POI POL POM POP POT POW POX PRO PRY PSI PUB PUD
   PUG PUL PUN PUP PUR PUS PUT PYA PYE PYX
   QAT QIS QUA
   RAD RAG RAH RAI RAJ RAM RAN RAP RAS RAT RAW RAX RAY
   REB REC RED REE REG REI REM REP RES RET REV REX RHO
   RIA RIB RID RIF RIG RIM RIN RIP ROB ROC ROD ROE ROM
   ROT ROW RUB RUE RUG RUM RUN RUT RYA RYE
   SAB SAC SAD SAE SAG SAL SAP SAT SAU SAW SAX SAY SEA
   SEC SEE SEG SEI SEL SEN SER SET SEW SEX SHA SHE SHH
   SHY SIB SIC SIM SIN SIP SIR SIS SIT SIX SKA SKI SKY
   SLY SOB SOD SOL SON SOP SOS SOT SOU SOW SOX SOY SPA
   SPY SRI STY SUB SUE SUM SUN SUP SUQ SYN
   TAB TAD TAE TAG TAJ TAM TAN TAO TAP TAR TAS TAT TAU
   TAV TAW TAX TEA TED TEE TEG TEL TEN TET TEW THE THO
   THY TIC TIE TIL TIN TIP TIS TIT TOD TOE TOG TOM TON
   TOO TOP TOR TOT TOW TOY TRY TSK TUB TUG TUI TUN TUP
   TUT TUX TWA TWO TYE
   UDO UGH UKE ULU UMM UMP UNS UPO UPS URB URD URN USE
   UTA UTS
   VAC VAN VAR VAS VAT VAU VAV VAW VEE VEG VET VEX VIA
   VIE VIG VIM VIS VOE VOW VOX VUG
   WAB WAD WAE WAG WAN WAP WAR WAS WAT WAW WAX WAY WEB
   WED WEE WEN WET WHA WHO WHY WIG WIN WIS WIT WIZ WOE
   WOK WON WOO WOP WOS WOT WOW WRY WUD WYE WYN
   XIS
   YAH YAK YAM YAP YAR YAW YAY YEA YEH YEN YEP YES YET
   YEW YID YIN YIP YOB YOD YOK YOM YON YOU YOW YUK YUM
   YUP
   ZAG ZAP ZAS ZAX ZED ZEE ZEK ZIG ZIN ZIP ZIT ZOA ZOO))

(defparameter *additional-words*
  (append *two-letter-words* *three-letter-words*))

(defvar *dictionary* nil
  "Either NIL (before it's initialized), or
a data structure that represents all the acceptable words,
as an array of length 16.
Words are represented by their lower-case string.
Index zero contains a hash-table with acceptable words as keys, T as value.
Indices 2 to 15 each contain a list, for each word sorted alphabetically,
of a pair of the word's lettercount and the word.")

(defun ensure-dictionary-present ()
  "Ensure the dictionary is present, using curl to download it if not"
  (unless (probe-file *dictionary-path*)
    (inferior-shell:run
     `(curl --output ,*dictionary-path* --compressed ,*dictionary-url*))))

(defun slurp-file-lines (file)
  "Return the contents of FILE as a list of one text string per line"
  (with-open-file (s file :direction :input :if-does-not-exist :error)
    (slurp-stream-lines s)))

(defun normalize-word (x)
  "Given a word, return a normalized form in lower case, no dash"
  (remove-if-not 'alpha-char-p (string-downcase x)))

(defun slurp-dictionary ()
  "Create the *dictionary* datastructure"
  (ensure-dictionary-present)
  (let* ((d (make-array '(16) :initial-element '()))
         (h (make-hash-table :test 'equal)))
    (loop :for word :in
      (append (slurp-file-lines *dictionary-path*) *additional-words*)
      :for w = (normalize-word word)
      :for l = (length w)
      :when (<= 2 l 15) :do
      (unless (gethash w h)
        (setf (gethash w h) t)
        (push w (aref d l))))
    (loop :for l :from 2 :to 15 :do
      (setf (aref d l)
            (mapcar (lambda (x) (cons (count-letters x) x))
                    (sort (aref d l) #'string<))))
    (setf (aref d 0) h)
    (setf *dictionary* d)
    (dictionary-size)))

(defun dictionary-size (&optional (dictionary *dictionary*))
  "Return the number of valid words in given dictionary"
  (loop :for l :from 2 :to 15 :sum (length (aref dictionary l))))

(defun valid-word-p (word &optional (dictionary *dictionary*))
  "Check whether a word is acceptable in the *dictionary*"
  (values (gethash word (aref dictionary 0))))

(defun map-dictionary (fun &key (min 2) (max 15) (dictionary *dictionary*))
  "For all words in the dictionary between length MIN and MAX,
apply FUN to the word's lettercount and the word"
  (when (<= min max)
    (loop :for length :from max :downto min :do
      (loop :for (lettercount . word) :in (aref dictionary length)
        :do (funcall fun lettercount word)))))

(defun ensure-dictionary ()
  "Make sure the dictionary has been initialized"
  (unless *dictionary* (slurp-dictionary)))

(deftype lettercount ()
  "Type for a LETTERCOUNT for a word or pattern:
An array of length 27, each containing a number from 0 to 15.
Indices 1 to 26 contain the number of occurrences for A to Z.
Index 0 contains the number of jokers available - for patterns"
  '(simple-array (unsigned-byte 8) (27)))

(defun make-lettercount ()
  "Make an uninitialized lettercount object"
  (make-array '(27) :element-type '(unsigned-byte 8)))

(defun letterp (x)
  "Is X a valid letter?"
  (and (characterp x) (char<= #\a x #\z)))

(defun letter-index (x &optional accept-jokers)
  "Given a letter X, return an index for that letter in a LETTERCOUNT array.
If ACCEPT-JOKERS, then X can also be #\. #\? or #\* representing a joker."
  (check-type x character)
  (cond
    ((letterp x)
     (- (char-code x) 96))
    ((and accept-jokers (find x "?.*"))
     0)
    (t
     (error "Invalid character ~C" x))))

(defun count-letters (x &optional accept-jokers)
  "Given a word or pattern X, return a corresponding LETTERCOUNT array
Patterns with jokers accepted if ACCEPT-JOKERS, otherwise only words"
  (check-type x string)
  (loop :with lc = (make-lettercount)
    :for l :across x
    :do (incf (aref lc (letter-index l accept-jokers)))
    :finally (return lc)))

(defun lettercount<= (x y)
  "Given two lettercounts X and Y, return true iff
the letters in X can all be represented using letters in Y"
  (check-type x lettercount)
  (check-type y lettercount)
  (loop :for l :from 1 :to 26 :with jokers = (- (aref y 0) (aref x 0))
    :for n = (- (aref x l) (aref y l))
    :always (or (<= n 0)
                (progn (decf jokers n)
                       (<= 0 jokers)))))

(defun letters<= (x y)
  "Given a word X and a pattern Y,
return true iff X can be written using letters in Y"
  (lettercount<= (count-letters x) (count-letters y t)))

(defun words-with-letters (x &key (min 2) (max (length x)) (max-only nil))
  "Given a pattern X, return all words between size MIN and MAX
that can be spelled out using letters in X.
If MAX-ONLY, only return those words
that are maximally long amongst the candidates"
  (ensure-dictionary)
  (let* ((lx (count-letters x t))
         (words
          (while-collecting (collect)
            (map-dictionary
             (lambda (letters word)
               (when (lettercount<= letters lx)
                 (collect word)))
             :min min :max max)))
         (p (when (and words max-only)
              (position (length (first words)) words
                        :key 'length :test-not '=))))
    (if p (subseq words 0 p) words)))

(defun show-words-with-letters (x &rest keys &key min max (max-only t))
  "Show all results from words-with-letters"
  (declare (ignore min max))
  (format t "~{~A~%~}" (apply 'words-with-letters x :max-only max-only keys))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *double-letter* :DL ;; :
    "Magic value of an empty square with letter-counts-double bonus")
  (defparameter *triple-letter* :TL ;; ∴
    "Magic value of an empty square with letter-counts-triple bonus")
  (defparameter *double-word* :DW ;; ²
    "Magic value of an empty square with word-counts-double bonus")
  (defparameter *triple-word* :TW ;; ³
    "Magic value of an empty square with word-counts-triple bonus")
  (defparameter *center* :** ;; ☆
    "Magic value of the empty center square"))

(deftype board ()
  "A board: an array containing pairs of a letter and a score value;
instead of a letter, NIL indicates an empty square,
and then the score value is either NIL or one of the magic
values indicating a center square or multiplier bonus."
  '(simple-array t (15 15)))

(defun make-blank-board ()
  "Make and return an empty board"
  (make-array '(15 15) :initial-element '(nil . nil)))

(defparameter *scrabble-tiles*
  '((*center* 7 7)
    (*triple-letter* 6 6)
    (*double-letter* 5 5)
    (*double-word* 4 4)
    (*double-word* 3 3)
    (*double-word* 2 2)
    (*double-word* 1 1)
    (*triple-word* 0 0)
    (*triple-word* 0 7)
    (*double-letter* 0 3)
    (*triple-letter* 1 5)
    (*double-letter* 2 6)
    (*double-letter* 3 7))
  "Special tile specification for Scrabble")

(defparameter *wwf-tiles*
  '((*center* 7 7)
    (*triple-word* 0 3)
    (*double-letter* 1 2)
    (*triple-letter* 0 6)
    (*double-word* 1 5)
    (*double-letter* 2 4)
    (*triple-letter* 3 3)
    (*double-word* 3 7)
    (*double-letter* 4 6)
    (*triple-letter* 5 5))
  "Special tile specification for Words With Friends")

(defun make-initial-board (&optional (game-spec :wwf))
  "Make and return an initial board for words-with-friends"
  (let ((b (make-blank-board))
	(tiles (ecase game-spec
		 (:scrabble *scrabble-tiles*)
		 (:wwf *wwf-tiles*))))
    (labels ((o (c x y) (setf (aref b x y) (cons nil c)))
             (o- (c x y) (o c x y) (o c (- 14 x) y))
             (o+ (c x y) (o- c x y) (o- c x (- 14 y)))
             (o* (c x y) (o+ c x y) (o+ c y x)))
      (loop :for (special x y) :in tiles :do
	(o* (symbol-value special) x y)))
    b))


(defun copy-board (board)
  "Copy a board data-structure"
  (let ((b (make-blank-board)))
    (loop :for x :below 15 :do
      (loop :for y :below 15 :do
        (setf (aref b x y) (aref board x y))))
    b))

(defparameter *board*
  (make-initial-board)
  "The global board datastructure being used")

(defun show-board (&optional (board *board*) (s t))
  "Display contents of BOARD on stream S"
  (with-output (s)
    (princ "  +" s)
    (loop :for column :below 15 :do
      (format s "-~2,'-D--" column))
    (format s "+~%") ;; ┌─┐
      (loop :for row :below 15 :do
        (format s "~2,' D|" row) ;; │
        (loop :for column :below 15
          :for (letter . value) = (aref board row column) :do
          (if letter
              (format s " ~3@<~:@(~A~)~D~> " letter value)
              (format s "  ~:[  ~;~:*~(~A~)~] " value)))
        (format s "|~%")) ;; │
      (princ "  +" s)
      (loop :repeat 75 :do (princ "-" s))
      (format s "+~%"))) ;; └─┘

(deftype direction ()
  "type for a direction following which to play a word:
:R for rightward, :D for downward"
  '(member :r :d))

(defun other-direction (x)
  (ecase x
    (:r :d)
    (:d :r)))

(defun bref (x y &optional (direction :r) (board *board*))
  "Lookup a square in the BOARD with first coordinate X and second coordinate Y
according to DIRECTION.
If it's :R, row then column; if it's :D, column then row."
  (when (and (<= 0 x 14) (<= 0 y 14))
    (ecase direction
      (:r (aref board x y))
      (:d (aref board y x)))))

(defun set-bref! (value x y &optional (direction :r) (board *board*))
  "Set the VALUE of the BOARD square
of coordinates X and Y according to DIRECTION, as per BREF"
  (assert (and (<= 0 x 14) (<= 0 y 14)))
    (ecase direction
      (:r (setf (aref board x y) value))
      (:d (setf (aref board y x) value))))

(defun (setf bref) (value x y &optional (direction :r) (board *board*))
  (set-bref! value x y direction board))

(defun brefp (x y &optional (direction :r) (board *board*))
  "Return the letter on a square of given coordinates as per BREF"
  (let ((lv (bref x y direction board)))
    (and lv (car lv))))

(defun bref-word (x y direction length &optional (board *board*))
  "Return a string with the letters out of the specified square (as per BREF),
in the specified direction, of the specified length.
Return NIL if the squares before or after said word are already played,
and fill unplayed squares with spaces."
  (and (<= 0 y) (<= (+ y length) 15)
       (not (brefp x (1- y) direction board))
       (not (brefp x (+ y length) direction board))
       (coerce (loop :for i :below length :collect
                 (or (car (bref x (+ y i) direction board)) #\space))
               'string)))

(defun bref-crossword (x y direction offset word &optional (board *board*))
  "Given a prospective word being played on the board,
return any crossword that would be formed at given offset,
together with the length of the prefix
before the prospective letter being played"
  (let* ((before (loop :for i :downfrom -1
                   :for r = (bref (+ x i) (+ y offset) direction board)
                   :for c = (and r (car r))
                   :while c :collect c))
         (after (loop :for i :from 1
                   :for r = (bref (+ x i) (+ y offset) direction board)
                   :for c = (and r (car r))
                   :while c :collect c)))
    (and (or before after)
         (values
          (coerce
           (append (reverse before) (list (aref word offset)) after)
           'string)
          (length before)))))

(defun first-move-p (&optional (board *board*))
  "Is this the first move in the game?"
  (equal *center* (cdr (aref board 7 7))))

(defun valid-move-position-p (x y direction length
                              &optional (board *board*) canonical-only)
  "Given a position as given by direction, coordinates, a length of word,
return true iff it is a valid position in which to play a new word at this move.
If CANONICAL-ONLY, don't return one-letter downward moves for which an equivalent
rightward move exists."
  (loop
    :with first-move-p = (first-move-p board)
    :with old-letter-count = 0
    :with by-center = nil
    :with has-crossword-p = nil
    :with template = (or (bref-word x y direction length board) (return nil))
    :with empties = (loop :for l :across template
                      :when (equal l #\space) :sum 1)
    :with () = (unless (<= 1 empties 7) (return nil))
    :for l :across template
    :for i :from 0 :do
    (cond
      ((equal l #\space)
       (when (or (brefp (- x 1) (+ y i) direction board)
                 (brefp (+ x 1) (+ y i) direction board))
         ;; TODO: maybe test for candidate crosswords in the dictionary?
         (setf has-crossword-p t))
       (when first-move-p
         (when (equal *center* (cdr (bref x (+ y i) direction board)))
           (setf by-center t))))
      (t
       (incf old-letter-count)))
    :finally
    (when (and canonical-only (= empties 1) (eq direction :d) has-crossword-p)
      (return nil))
    (return (if first-move-p
                by-center
                (or (plusp old-letter-count) has-crossword-p)))))

(defun valid-move-p (x y direction word
                     &optional (board *board*) (checker #'valid-word-p))
  "Assuming (valid-move-position-p x y direction (length word) board nil),
does the given WORD fit in the given position?"
  (and (funcall checker word)
       (loop
         :for l :across (bref-word x y direction (length word) board)
         :for i :from 0 :do
         (cond
           ((equal l #\space)
            (let ((crossword (bref-crossword x y direction i word board)))
              (when crossword
                (unless (funcall checker crossword)
                  (return nil)))))
           ((equal l (aref word i))
            nil)
           (t
            (return nil)))
         :finally (return t))))

(defparameter *bingo-bonus* 35
  "Bonus for a bingo (using all letters)")

(defun score-move-internal (x y direction word &optional (board *board*))
  "Helper function to compute the score for a valid move;
returns three values:
the SCORE assuming none of the letters in word is a joker,
an array of LETTER-VALUES specifying for the letter at each offset
by how many points to decrease the score if said letter were a joker, and
a list of crosswords spelled out while playing the WORD.
Assumes (valid-move-p x y direction word board)."
  (declare (optimize (speed 1) (debug 3) (safety 3)))
  (flet ((a () (make-array (length word)
                           :element-type 'fixnum :initial-element 0)))
    (loop :with word-value = 0
      :with crosswords-value = 0
      :with main-multiplier = 1
      :with direct-multiplier = (a)
      :with cross-multiplier = (a)
      :with letter-values = (a)
      :with new-letters = 0
      :with crosswords = ()
      :for i :from 0 :below (length word)
      :for (l . v) = (bref x (+ y i) direction board) :do
      (if l
          (incf word-value v) ;; previously played letter
          (let ((letter-multiplier
                 (case v ;; letter multiplier premium square?
                   (#.*triple-letter* 3)
                   (#.*double-letter* 2)
                   (otherwise 1)))
                (word-multiplier
                 (case v ;; word multiplier premium square?
                   (#.*triple-word* 3)
                   (#.*double-word* 2)
                   (#.*center* 2) ;; in Scrabble. Is it also the same in WWF ?
                   (otherwise 1))))
            (incf new-letters)
            (multiple-value-bind (crossword offset)
                (bref-crossword x y direction i word board)
              (when crossword
                (push crossword crosswords)
                (incf crosswords-value
                      (* word-multiplier
                         (loop :for j :from (- offset) :repeat (length crossword)
                           :unless (= j 0)
                           :sum (cdr (bref (+ x j) (+ y i) direction board)))))
                (setf (aref cross-multiplier i)
                      (* 1 word-multiplier letter-multiplier))))
            (setf main-multiplier (* main-multiplier word-multiplier))
            (setf (aref direct-multiplier i) letter-multiplier)))
    :finally
      (return
        (loop
          :for c :across word
          :for i :from 0
          :for base-letter-value = (aref *letter-values* (letter-index c))
          :for letter-value = (* base-letter-value
                                 (+ (* main-multiplier (aref direct-multiplier i))
                                    (aref cross-multiplier i)))
          :sum letter-value :into new-value
          :do (setf (aref letter-values i) letter-value)
          :finally (return
                     (values (+ new-value
                                (* main-multiplier word-value)
                                crosswords-value
                                (if (= 7 new-letters) *bingo-bonus* 0))
                             letter-values
                             (nreverse crosswords))))))))

(defun adjust-score (score letter-values joker)
  "Given a score and letter-values as per SCORE-MOVE-INTERNAL,
and given JOKER positions, return actual SCORE for a given move"
  (unless (listp joker)
    (setf joker (list joker)))
  (- score
     (loop :for j :in joker :sum (aref letter-values j))))

(defun score-move (x y direction word &key (board *board*) joker)
  "Return score for a prospective move (assumed valid)"
  (multiple-value-bind (score letter-values)
      (score-move-internal x y direction word board)
    (adjust-score score letter-values joker)))

(defun all-move-scores (x y direction word letters &key (board *board*))
  "Given a move, return for each potential joker placement
a list of the score made by playing the move with said joker placement,
a list of the coordinates, direction, word spelled out,
and joker placements if any, and any crosswords spelled out"
  (multiple-value-bind (score letter-values crosswords)
      (score-move-internal x y direction word board)
    (loop :with template = (bref-word x y direction (length word) board)
      :for jokers :in (generate-joker-positions word letters template)
      :for s = (adjust-score score letter-values jokers)
      :collect (list* s (list* x y direction word
                               (when jokers (list :joker jokers)))
                      crosswords))))

;;; multiset as alist of element (compared with equal) to integer count
(defun a-mset-remove-one (letter a-mset)
  "Given a multiset of letters, remove one occurrence of given LETTER"
  (loop :for (elem . rest) :on a-mset
    :for (l . n) = elem
    :until (equal l letter)
    :collect elem :into unmatched
    :finally
    (return (append unmatched
                    (let ((m (1- n))) (unless (zerop m) (list (cons l m))))
                    rest))))

(defun a-mset-remove-all (letter a-mset)
  "Given a multiset of letters, remove all occurrences of given letter"
  (remove letter a-mset :test #'equal :key #'car))

(defun a-mset-add (letter a-mset)
  "Given a multiset of letters, add one occurrence of given letter"
  (loop :for (elem . rest) :on a-mset
    :for (l . n) = elem
    :when (equal l letter)
      :do (return (append unmatched (list (cons l (1+ n))) rest))
    :else :collect elem :into unmatched :end
    :finally (return (cons (cons letter 1) a-mset))))

(defun a-mset-ref (letter a-mset)
  "Given a multiset of letters and a letter, return number of occurrences if any"
  (cdr (assoc letter a-mset :test #'equal)))

(defun a-mset-size (a-mset)
  "Given a multiset of letters, return the overall number of letters"
  (loop :for (() . n) :in a-mset :sum n))

(defun generate-joker-positions (word letters template)
  "Given WORD, a set of LETTERS to play, and a TEMPLATE
telling which letters are already on the board with spaces to fill,
generate all the joker positions to play said WORD with said LETTERS"
  (let* ((lettercount (count-letters letters t))
         (n-jokers (aref lettercount 0))
         (*1 (when (zerop n-jokers)
              (return-from generate-joker-positions '(()))))
         (wordcount (count-letters word))
         (*2 (loop :for l :across template
               :unless (equal l #\space)
               :do (decf (aref wordcount (letter-index l)))))
         (jokers-needed
          (loop :for i :from 1 :to 26
            :when (> (aref wordcount i) (aref lettercount i))
            :collect (cons i (- (aref wordcount i) (aref lettercount i)))))
         (n-jokers-needed
          (a-mset-size jokers-needed))
         (remaining
          (loop :for i :from 0
            :for c :across word
            :for l :across template
            :when (equal l #\space)
            :collect (cons (letter-index c) i)))
         (needed-remaining
          (loop :with n = ()
            :for (c . i) :in remaining
            :when (a-mset-ref c jokers-needed)
            :do (setf n (a-mset-add c n))
            :finally (return n))))
    (declare (ignore *1 *2))
    (while-collecting (c)
      (labels ((choose-jokers (chosen n-jokers
                               needed n-needed
                               remaining n-remaining
                               needed-remaining)
                   (when (zerop n-needed)
                     (c (reverse chosen)))
                   (choose-jokers+
                    chosen n-jokers
                    needed n-needed
                    remaining n-remaining
                    needed-remaining))
               (choose-jokers+ (chosen n-jokers
                                needed n-needed
                                remaining n-remaining
                                needed-remaining)
                 (when (<= n-needed n-jokers n-remaining)
                   (when remaining
                     (destructuring-bind ((c . i) . rest) remaining
                       (let ((n (a-mset-ref c needed)))
                         (cond
                           (n
                            (when (< n (a-mset-ref c needed-remaining))
                              (choose-jokers+
                               chosen n-jokers needed n-needed
                               rest (1- n-remaining)
                               (a-mset-remove-one c needed-remaining)))
                            (when (plusp n-jokers)
                              (choose-jokers
                               (cons i chosen) (1- n-jokers)
                               (a-mset-remove-one c needed) (1- n-needed)
                               rest (1- n-remaining)
                               (if (= 1 n)
                                   (a-mset-remove-all c needed-remaining)
                                   (a-mset-remove-one c needed-remaining)))))
                           (t
                            (choose-jokers+ chosen n-jokers needed n-needed
                                            rest (1- n-remaining)
                                            needed-remaining)
                            (when (plusp n-jokers)
                              (choose-jokers (cons i chosen) (1- n-jokers)
                                             needed n-needed
                                             rest (1- n-remaining)
                                             needed-remaining))))))))))
        (choose-jokers '() n-jokers jokers-needed n-jokers-needed
                       remaining (length remaining) needed-remaining)))))

(defun possible-moves-at (x y direction length letters
                          &optional (board *board*) canonical-only)
  "Return all possible moves at the given position, scored as per SCORE-MOVE"
  (when (valid-move-position-p x y direction length board canonical-only)
    (loop :with previous = (remove #\space
                                   (bref-word x y direction length board))
      :for word :in (words-with-letters (concatenate 'string previous letters)
                                        :min length :max length :max-only nil)
      :when (valid-move-p x y direction word board)
      :nconc (all-move-scores x y direction word letters :board board))))

(defun map-move-positions (fun)
  "Apply FUN to all possible move positions on a board"
  (loop :for direction :in '(:r :d) :do
    (loop :for x :from 0 :below 15 :do
      (loop :for y :from 0 :below 13 :do
        (loop :for length :from 2 :to (- 15 y) :do
          (funcall fun x y direction length))))))

(defun possible-moves (letters &optional (board *board*))
  "Compute all possible moves for a bag of LETTERS on the BOARD"
  (sort
   (while-collecting (c)
     (map-move-positions
      (lambda (x y direction length)
        (map () #'c
             (possible-moves-at x y direction length letters board t)))))
   #'< :key 'car))

(defparameter *previous-game* ()
  "Saved history of the previous game")

(defparameter *player* 0
  "Who is the current player? 0 (start player) or 1")
(defparameter *score* '(0 0)
  "Which player has which score?")

(defun save-game ()
  "return a datastructure to save the state of the current game"
  (list *player* *score* (copy-board *board*)))
(defun restore-game (game)
  "restore the state of the current game from a datastructure"
  (destructuring-bind (player score board) game
    (setf *player* player
          *score* score
          *board* board)
    (show-game)))
(defun show-game ()
  "Show the current state of the game"
  (show-board)
  (format t "player ~A to move. Score: ~{~A ~A~}~%" *player* *score*)
  (finish-output))
(defun reset-game (&optional game-spec)
  "Reset game to initial state."
  (ensure-dictionary)
  (setf *board* (make-initial-board game-spec)
        *player* 0
        *score* '(0 0)))

(defun play-at (x y direction word
                &key (board *board*) joker (update-board (eq board *board*)))
  "Modify current game state as per WORD being played at given position;
update the board; show the new state; return the score"
  (assert (valid-move-position-p x y direction (length word) board))
  (assert (valid-move-p x y direction word board (constantly t)))
  (unless (listp joker)
    (setf joker (list joker)))
  (let* ((score (score-move x y direction word :board board :joker joker))
         (new-words (while-collecting (w)
                      (unless (valid-move-p x y direction word board
                                            (lambda (x)
                                              (unless (valid-word-p x)
                                                (w x))
                                              t))
                        (error "invalid move")))))
    (when new-words
      (format t "New words:~{ ~A~}~%" new-words))
    (when update-board
      (setf *previous-game* (save-game))
      (setf *board* (copy-board board))
      (setf board *board*)
      (setf *score* (copy-list *score*))
      (incf (nth *player* *score*) score)
      (setf *player* (- 1 *player*)))
    (loop
      :for c :across word
      :for i :from 0
      :for (l . v) = (bref x (+ y i) direction board)
      :unless l :do
      (set-bref! (cons c
                       (if (member i joker)
                           0
                           (aref *letter-values* (letter-index c))))
                 x (+ y i) direction board))
    (when update-board
      (show-game))
    score))

(defun do-play-game (game-spec moves letters)
  "Given a list of moves as arguments on which to apply PLAY-AT,
and a set of LETTERS, list possible moves."
  (reset-game game-spec)
  (dolist (move moves)
    (apply 'play-at move))
  (possible-moves letters))

(defmacro play-game (&rest game)
  "Given a list of moves followed by a set of LETTERS as string,
re-play the game and list possible next moves"
  (let ((game-spec (if (keywordp (car game)) (pop game) :wwf))
	(letters (car (last game)))
	(moves (butlast game)))
  `(do-play-game ,game-spec ',moves ,letters)))
