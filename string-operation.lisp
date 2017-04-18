(defparameter *mystr* (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character))

(defun push-character(arg goal)
  (vector-push (character arg) goal))

(defun getchared()
  (loop for getchar across "qwerasdf" collect getchar))  ;->(#\q #\w #\e #\r #\a #\s #\d #\f)

(defun collect-character(string)
  (coerce string 'character))   ;string -> "a"   -> #\a  length of string must be one

(defun change-to-char(string)
  (coerce string 'list))                     ;->(#\q #\w #\e #\r #\a #\s #\d #\f)

(defun get-character(character-input)
  (character character-input))    ; character-input must be a symbol such as 'a or 'b

(defun string-append(stringa stringb &optional starta  enda startb endb )
  (if starta
      starta
      (setf starta 0))
  (if startb
      startb
      (setf startb 0))
  (if enda
      enda
      (setf enda (length stringa)))
  (if endb 
      (setf endb (length stringb)))
  (if (and
       (<= starta (length stringa))
       (<= startb (length stringb))
       (<= enda (length stringa))
       (<= endb (length stringb)))
      (concatenate 'string
		   (subseq stringa starta enda)
		   (subseq stringb startb endb))
      "string-append arguments error"))

(defun get-string()
  (format nil "~A" (read)))
