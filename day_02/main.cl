(defun make-movie (title year genre director)
  (list :title title :year year :genre genre :director director))

(defun get-title (list)
  (getf list :title))

(defun get-year (list)
  (getf list :year))

(defun get-genre (list)
  (getf list :genre))

(defun get-director (list)
  (getf list :director))

(defvar *movies* nil)

(defun add-movie (movie)
  (push movie *movies*))

(defun print-movies ()
  (dolist (movie *movies*)
	(format t "~{~a:~10t~a~%~}~%" movie)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-movie ()
  (make-movie
	(prompt-read "Title")
	(or (parse-integer (prompt-read "Year") :junk-allowed t) -1)
	(prompt-read "Genre")
	(prompt-read "Director")))

