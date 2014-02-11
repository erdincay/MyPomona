;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Get IDE from http://racket-lang.org/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The program erases all the comments as "(** anything *)" in a file, 
;; but keep some special comments as "(** **** anything *)" for readable reason.
;;I think Regular Expression itself can match "(** anything *)" meanwhile exclude "(** **** anything *)" 
;; without using the backup->placeholder->restore tactic below, which is a little ugly.
;;But I am a little lazy to research Regular Expression that deep. 
;;So if anyone can solve the problem without the backup tactic please let me know: kding@csupomona.edu.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define input-name "file_path/file_name.v")

(define (erase-some-comments input-filename)
  (let ((placeholder "(&& &)")
        (backupReg #px"\\([\\*]{2}.[\\*]{4}(.|[\r\n])*?\\*\\)") ;;backup regex to match (** **** anything *)
        (replaceReg #px"\\([\\*]+(.|[\r\n])*?\\*\\)") ;;replaceReg regex to match (** anything *)
        (str-file (file->string input-name #:mode'binary))) 
    (let ((backuplist (regexp-match* backupReg str-file))
          (holder-file (string-replace str-file backupReg placeholder)));;replace readable comments with placeholder
      
      ;;remove all comments
      (define (remove-comments regex file)
        (string-replace file regex ""))
      
      ;;restroe placeholder with backup
      (define (restore input list)
        (cond
          ((null? (cdr list))
           (string-replace input placeholder (car list) #:all?'#f))
          (else 
           (restore (string-replace input placeholder (car list) #:all?'#f) (cdr list)))))
      
      ;;change file name
      (define (output-name name)
        (define str-list (string-split name "." #:trim?'#t))
        (string-join str-list "." #:before-last "_no_comments."))
      
      (let ((outfile (open-output-file (output-name input-name) #:mode'binary #:exists'replace))
            (strlist (string-split (restore (remove-comments replaceReg holder-file) backuplist) "\r\n" #:trim?'#t)))
        
        ;;write new file
        (define (write-list list out)
          (cond
            ((null? list) 'done)
            ((equal? (string-trim (car list)) "") (write-list (cdr list) out))
            (else (if (regexp-match backupReg (car list))
                      (begin
                        (newline out)
                        (displayln "(** ########################################################################################################### *)" out))
                      'none)
                  (displayln (car list) out)
                  (write-list (cdr list) out))))
        
        (write-list strlist outfile)
        (close-output-port outfile)))))

(erase-some-comments input-name)