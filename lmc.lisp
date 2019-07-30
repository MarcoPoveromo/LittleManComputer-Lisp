;;;; -*- Mode: Lisp -*-

;;;; Poveromo Marco 830626
;;;; Zorat Lorenzo 830641
;;;; Fermini Simone 830748

;;;; lmc.lisp --

;;; read-list-from --
;;; Legge il file e lo suddivide in righe. Ogni elemento della lista è una riga.
(defun read-list-from (input-stream)
  (let ((e (read-line input-stream nil 'eof)))
    (unless (eq e 'eof)
      (cons e (read-list-from input-stream)))))

;;; open-file --
;;; Apre il file in lettura e invoca la funzione read-list-from
(defun open-file (Dir)
  (with-open-file (in Dir
                      :direction :input
                      :if-does-not-exist :error)
    (read-list-from in)))

;;; remove-comments --
;;; Rimuove tutti i commenti, dalla lista l, e restiuisce il file senza commenti
(defun remove-comments (l)
  (if (eql nil l) nil
    (cons (string-trim " " (first (split-sequence "//" (first l)))) 
          (remove-comments (rest l)))))

;;; empty-string --
;;; Se la lunghezza della stringa str è 0 risponde true, altrimenti nil
(defun empty-string (str)
  (if (eq (length str) 0) T nil))

;;; remove-comment-emptylines --
;;; Rimuove tutte le righe vuote dalla lista corrispondente al file
(defun remove-comment-emptylines (Dir)
  (delete-if 'empty-string (remove-comments (open-file Dir))))

;;; label-picker --
;;; Prende tutte le etichette presenti nella lista l corrispondente al file asm
;;; e restituisce una lista contenente cons-cell con le etichette e la riga
;;; di riferimento. ES: (("label1" . 0) (...) (...))
(defun label-picker (l n-line)
  (if (eql nil l) nil
    (cons (label-picker-line (car l) n-line)
          (label-picker (cdr l) (+ n-line 1)))))

;;; label-picker-line --
;;; Funzione helper di label-picker: prende una singola riga e il numero di riga
;;; e resituisce una cons-cell. ES: ("label" . 3)
(defun label-picker-line (line n-line)
  (let ((a (split-sequence " " line :coalesce-separators t)))
    (cond ((and (= (length a) 3)
                (not (numberp (parse-integer
                               (subseq (first a) 0 1)
                               :junk-allowed t))))
           (cons (string-downcase (first a)) n-line))
          ((and (= (length a) 2) 
                (is-istr-a (string-downcase (second a)))
                (not (numberp (parse-integer
                               (subseq (first a) 0 1)
                               :junk-allowed t))))
           (cons (string-downcase (first a)) 
                 n-line))
          ((and (= (length a) 3) (numberp (parse-integer
                                           (subseq (first a) 0 1)
                                           :junk-allowed t)))
           (error "Le label non possono iniziare con un numero."))
          ((and (= (length a) 2) 
                 (is-istr-a (string-downcase (second a)))
                (numberp (parse-integer
                          (subseq (first a) 0 1)
                          :junk-allowed t)))
            (error "Le label non possono iniziare con un numero."))
          (T nil))))

;;; is-istr-a --
;;; Sono il primo tipo di istruzione quindi hlt, dat, out, inp e 
;;; resituisce true se è uno di questi tipi di istruzione
(defun is-istr-a (istr)
  (cond ((equal istr "hlt") T)
        ((equal istr "dat") T)
        ((equal istr "out") T)
        ((equal istr "inp") T)
        (T nil)))

;;; is-nil --
(defun is-nil (x)
  (if (eql x nil) t nil))

;;; label-remover --
;;; Rimuove tutte le label e ritorna il testo del file asm senza le label
(defun label-remover (l)
  (if (eql l nil) nil
    (cons (label-remover-line (car l)) 
          (label-remover (cdr l)))))

;;; label-remover-line --
;;; Prende una riga sola e rimuove le label solamente da quella singola riga
(defun label-remover-line (line)
  (let ((a (split-sequence " " line :coalesce-separators t)))
    (cond ((= (length a) 3) ;Lunghezza uguale a 3 - "LABEL" "ISTR" "NUM/LABEL"
           (concatenate 'string (second a) " " (third a)))
          ((and (= (length a) 2) ;Lunghezza uguale a 2 - "LABEL" "ISTR(A)"
                (is-istr-a (string-downcase (second a))))
           (second a))
          ((= (length a) 2) ;Lunghezza uguale a 2 - "ISTR" "NUM/LABEL"
           (concatenate 'string (first a) " " (second a)))
          ((= (length a) 1) ;Lunghezza uguale a 1 - "ISTR"
           (first a)))))

;;; create-memory --
;;; Scorre tutto il testo del file asm traducendo tutte le istruzioni 
;;; presenti in numero andando così a creare la memoria
(defun create-memory (no-label-text label-num)
  (if (eql no-label-text nil) nil
    (cons (create-memory-cell (car no-label-text) 
                              label-num)
          (create-memory (cdr no-label-text) 
                         label-num))))

;;; create-memory-cell --
;;; Funzione helper di create-memory: prende una riga del testo del 
;;; file asm e la converte in numero
(defun create-memory-cell (line label-num-list)
  (let ((a (split-sequence " " line :coalesce-separators t)))
    (cond ((and (= (length a) 1) ;Lunghezza uguale a 1 - HLT, INP, OUT, DAT
                (istr-a (string-downcase (first a))))
           (istr-a (string-downcase (first a))))
          ((and (= (length a) 2) ;Lunghezza uguale a 2 - DAT
                (equal (string-downcase (first a)) "dat")
                (parse-integer (second a) :junk-allowed t)
                (< (parse-integer (second a)) 1000)
                (>= (parse-integer (second a)) 0))
           (parse-integer (second a)))
          ((and (= (length a) 2) ;Lunghezza uguale a 2 - "ISTR" "NUMERO"
                (parse-integer (second a) :junk-allowed t)
                (< (parse-integer (second a)) 100)
                (>= (parse-integer (second a)) 0))
           (+ (istr-b (string-downcase (first a)))
              (parse-integer (second a))))
          ((and (= (length a) 2) ;Lunghezza uguale a 2 - "ISTR" "LABEL"
                (is-in (string-downcase (second a)) label-num-list)
                (istr-b (string-downcase (first a))))
           (+ (is-in (string-downcase (second a)) label-num-list)
              (istr-b (string-downcase (first a)))))
          (T (error "Il file asm contiene istruzioni non valide.")))))

;;; is-in --
;;; Prendendo in input una stringa e la lista con le associazioni
;;; ("label" . num-line) e, dopo aver controllato se la stringa e' presente
;;; ritorna il numero. (Se e' presente, sicuramente e' una label)
(defun is-in (label label-num-list)    
  (cond ((eql nil label-num-list) nil)
        ((equal label (car (car label-num-list)))
         (cdr (car label-num-list)))
        (T (is-in label
                  (cdr label-num-list))))) 

;;; istr-a --
;;; Ritorna il codice dell'istuzione 
;;; Se l'istruzione è di tipo inp, dat, out hlt
(defun istr-a (istr)
  (cond ((equal istr "inp") 901)
        ((equal istr "out") 902)
        ((equal istr "hlt") 0)
        ((equal istr "dat") 0)))

;;; istr-b --
;;; Ritorna il codice dell'istruzione 
;;; Se l'istuzione è due elementi e quindi di tipo add xx, sub xx, 
;;; sta xx, lda xx, bra xx, brz xx, brp xx
(defun istr-b (istr)
  (cond ((equal istr "add") 100)
        ((equal istr "sub") 200)
        ((equal istr "sta") 300)
        ((equal istr "lda") 500)
        ((equal istr "bra") 600)
        ((equal istr "brz") 700)
        ((equal istr "brp") 800)))

;;; duplicate-labels --
;;; Controlla se esistono label duplicate, questo non è possibile.
;;; In questo caso viene generato errore nella funzione chiamante.
(defun duplicate-labels (label-num-list)
  (cond ((eql label-num-list nil) nil)
        ((is-in (car (car label-num-list))
                (cdr label-num-list)) T)
        (T (duplicate-labels (cdr label-num-list)))))

;;; padding-mem --
;;; Riempie il contenuto della memoria nelle restanti celle vuote (con 0)
(defun padding-mem (mem n)
  (cond ((> n 100) (error "Memory overflow (> 100)."))
        ((= n 100) mem)
        ((< n 100) (append mem (make-list (- 100 n) :initial-element 0)))))

;;; lmc-load --
;;; Legge un file di testo e restituisce la rispettiva traduzione del 
;;; codice assembly in una lista di istruzioni macchina per lmc (Base 10).
(defun lmc-load (Dir)
  (let* ((no-comment-text ;File senza commenti e linee vuote 
          (remove-comment-emptylines Dir))
         ;; Lista con associazione tra label e num riga 
         ;; Esempio: (("label" . 2) ("label1" . 5) (...)) 
         (label-num-list 
          (delete-if 'is-nil (label-picker no-comment-text 0)))
         (a (if (duplicate-labels label-num-list) 
                (error "Label duplicate.")))
         (no-label-text (label-remover no-comment-text))
         (mem (create-memory no-label-text 
                             label-num-list))
         (mem-padded (padding-mem mem 
                                  (length mem))))
    mem-padded))

;;; lmc-run --
;;; Legge un file in input e restituisce la lista di output finale
(defun lmc-run (dir input)
  (if (list-ok input)
      (execution-loop (list 'state 
                            :acc 0 
                            :pc 0 
                            :mem (lmc-load dir) 
                            :in input 
                            :out () 
                            :flag 'noflag))
    (error "Input non valido.")))

;;; is-state --
;;; Controlla se lo state rispetta i parametri richiesti dal testo
(defun is-state (state)
  (if (and (eq (first state) 'state)
           (eq (second state) ':acc)
           (eq (fourth state) ':pc)
           (eq (sixth state) ':mem)
           (eq (eighth state) ':in)
           (eq (tenth state) ':out)
           (eq (nth 11 state) ':flag))
      (if (and (>= (third state) 0)
               (< (third state) 1000)) ;Controllo ACC
          (if (and (>= (fifth state) 0) 
                   (< (fifth state) 100)) ;Controllo PC
              (if (and (eq (length (seventh state)) 100) 
                       (list-ok (seventh state))) ;Controllo MEM
                  (if (list-ok (ninth state)) ;Controllo IN
                      (if (list-ok (nth 10 state)) ;Controllo OUT
                          (if (or (eq (nth 12 state) 'flag) 
                                  (eq (nth 12 state) 'noflag)) T)))))))) ;FLAG


;;; is-halted-state --
;;; Controlla se l'halted-state rispetta i parametri richiesti dal testo
(defun is-halted-state (state)
  (if (and (eq (first state) 'halted-state)
           (eq (second state) ':acc)
           (eq (fourth state) ':pc)
           (eq (sixth state) ':mem)
           (eq (eighth state) ':in)
           (eq (tenth state) ':out)
           (eq (nth 11 state) ':flag))
      (if (and (>= (third state) 0) 
               (< (third state) 1000)) ;Controllo ACC
          (if (and (>= (fifth state) 0)
                   (< (fifth state) 100)) ;Controllo PC
              (if (and (eq (length (seventh state)) 100)
                       (list-ok (seventh state))) ;Controllo MEM
                  (if (list-ok (ninth state)) ;Controllo IN
                      (if (list-ok (nth 10 state)) ;Controllo OUT
                          (if (or (eq (nth 12 state) 'flag) 
                                  (eq (nth 12 state) 'noflag)) T)))))))) ;FLAG

;;; list-ok --
;;; Controlla ricorsivamente se gli elementi della lista rispettano 
;;; i parametri richiesti dal testo
(defun list-ok (lista)
  (cond ((null lista) T) ;Se la lista è vuota
        ((< (first lista) 0) nil) 
        ((>= (first lista) 1000) nil)
        (t (list-ok (rest lista)))))

;;; one-instruction --
;;; La funzione corrisponde all'esecuzione di una singola istruzione
;;; sulla memoria. Restituisce un nuovo stato generato dopo l'esecuzione
;;; della rispettiva operazione
(defun one-instruction (state)
  (let((acc (third state))
       (pc (fifth state))
       (mem (seventh state))
       (in (nth 8 state))
       (out (nth 10 state))
       (flag (nth 12 state))
       (istr (nth (fifth state) 
                  (seventh state))))
    (cond 
     ;;HALT
     ((< istr 100)
      (list 'halted-state 
            :acc acc 
            :pc pc 
            :mem mem
            :in in 
            :out out 
            :flag flag))
     ;;ADDIZIONE, con risultato < 1000
     ((and (>= istr 100) 
           (< istr 200) 
           (< (+ acc (nth (- istr 100) mem)) 1000))
      (list 'state 
            :acc (+ acc (nth (- istr 100) mem)) 
            :pc (+ pc 1) 
            :mem mem 
            :in in 
            :out out 
            :flag flag))
     ;;ADDIZIONE, con risultato >= 1000
     ((and (>= istr 100) 
           (< istr 200) 
           (>= (+ acc (nth (- istr 100) mem)) 1000))
      (list 'state 
            :acc (mod (+ acc (nth (- istr 100) mem)) 1000) 
            :pc (+ pc 1) 
            :mem mem 
            :in in 
            :out out 
            :flag 'flag))
     ;;SOTTRAZIONE, con risultato >= 0
     ((and (>= istr 200) 
           (< istr 300)
           (>= (- acc (nth (- istr 200) mem)) 0))
      (list 'state 
            :acc (- acc (nth (- istr 200) mem)) 
            :pc (+ pc 1) 
            :mem mem 
            :in in 
            :out out
            :flag flag))
     ;;SOTTRAZIONE, con risultato < 0
     ((and (>= istr 200)
           (< istr 300)
           (< (- acc (nth (- istr 200) mem)) 0))
      (list 'state 
            :acc (mod (- acc (nth (- istr 200) mem)) 1000) 
            :pc (+ pc 1) 
            :mem mem 
            :in in 
            :out out 
            :flag 'flag))
     ;;STORE
     ((and (>= istr 300)
           (< istr 400))
      (list 'state 
            :acc acc 
            :pc (+ pc 1) 
            :mem (substitute-element mem (- istr 300) acc)
            :in in 
            :out out 
            :flag flag))
     ;;LOAD
     ((and (>= istr 500)
           (< istr 600))
      (list 'state 
            :acc (nth (- istr 500) mem) 
            :pc (+ pc 1)
            :mem mem 
            :in in 
            :out out 
            :flag flag))
     ;;BRANCH
     ((and (>= istr 600) 
           (< istr 700))
      (list 'state 
            :acc acc 
            :pc (- istr 600) 
            :mem mem 
            :in in 
            :out out
            :flag flag))
     ;;BRANCH IF ZERO: Caso 1
     ((and (>= istr 700)
           (< istr 800)
           (eq acc 0)
           (eq flag 'noflag))
      (list 'state 
            :acc acc 
            :pc (- istr 700) 
            :mem mem 
            :in in
            :out out
            :flag flag))
     ;;BRANCH IF ZERO: Caso 2
     ((and (>= istr 700)
           (< istr 800)
           (or (> acc 0) 
               (eq flag 'flag)))
      (list 'state
            :acc acc
            :pc (+ pc 1)
            :mem mem
            :in in 
            :out out
            :flag flag))
     ;;BRANCH IF POSITIVE: Caso 1
     ((and (>= istr 800) 
           (< istr 900) 
           (eq flag 'noflag))
      (list 'state
            :acc acc 
            :pc (- istr 800) 
            :mem mem
            :in in
            :out out
            :flag flag))
     ;;BRANCH IF POSITIVE: Caso 2
     ((and (>= istr 800)
           (< istr 900)
           (eq flag 'flag))
      (list 'state
            :acc acc 
            :pc (+ pc 1) 
            :mem mem
            :in in
            :out out
            :flag flag))
     ;;INPUT
     ((and (eq istr 901) 
           (> (length in) 0))
      (list 'state 
            :acc (nth 0 in) 
            :pc (+ pc 1) 
            :mem mem 
            :in (rest in)
            :out out
            :flag flag))
     ;;OUTPUT
     ((eq istr 902)
      (list 'state 
            :acc acc 
            :pc (+ pc 1) 
            :mem mem 
            :in in 
            :out (append out (list acc)) 
            :flag flag))
     ;;ISTRUZIONE NON VALIDA
     (t nil))))

;;; substitue-element --
;;; Sostituisce l'elemento nell'n-esima posizione della memoria con acc
(defun substitute-element (mem n acc)
  (cond ((= n 0) 
         (cons acc 
               (rest mem)))
        (t (cons (first mem) 
                 (substitute-element (rest mem) (- n 1) acc)))))

;;; execution-loop --
;;; Esegue tutte le one-instruction necessarie per completare il programma
;;; e restituisce in output la coda di output quando raggiunge l'halted-state
(defun execution-loop (state)
  (cond ((is-state state) 
         (execution-loop (one-instruction state)))
        ((is-halted-state state)
         (nth 10 state))
        ((eq state nil)
         (error "Istruzione non valida."))))

       
;;;; End of file: lmc.lisp
