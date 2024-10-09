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

(defun save-movies (filename)
  (with-open-file (out filename
					   :direction :output
					   :if-exists :supersede)
	(with-standard-io-syntax
	  (print *movies* out))))

(defun load-movies (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax
	  (setf *movies* (read in)))))

(defun select-movie-by-year (value)
  (remove-if-not #'(lambda (movie) (equal (getf movie :year) value))
				 *movies*))

(defun select (selector-fn)
  (remove-if-not selector-fn *movies*))

(defun title-selector (title)
  #'(lambda (movie) (equal (getf movie :title) title)))

(defun year-selector (year)
  #'(lambda (movie) (equal (getf movie :year) year)))

(defun genre-selector (genre)
  #'(lambda (movie) (equal (getf movie :genre) genre)))

(defun director-selector (director)
  #'(lambda (movie) (equal (getf movie :director) director)))

;;

(defun where (&key title year genre director)
  #'(lambda (movie)
	  (and 
		(if title (equal (getf movie :title) title) t)
		(if year (equal (getf movie :year) year) t)
		(if genre (equal (getf movie :genre) genre) t)
		(if director (equal (getf movie :director) director) t))))

(defun update (selector-fn &key title year genre director)
  (setf *movies*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if year   	(setf (getf row :year) year))
               (if genre   	(setf (getf row :genre) genre))
               (if director (setf (getf row :director) director)))
             row) *movies*)))

(defun delete-rows (selector-fn)
  (setf *movies* (remove-if selector-fn *movies*)))

(defun comp-gn (field value)
 `(equal (getf movie ,field) ,value)) 

(defun comp-list-gn (fields)
  (loop while fields
		collecting (comp-gn (pop fields) (pop fields))))

(defmacro where1 (&rest clauses)
  `#'(lambda (movie) (and ,@(comp-list-gn clauses))))
