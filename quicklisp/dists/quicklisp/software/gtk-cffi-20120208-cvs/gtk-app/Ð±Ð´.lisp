

(in-package :бд)



(defvar *поля* (make-hash-table)
  "Hash table: класс -> описание реквизитов")

(defvar *формы* (make-hash-table)
  "Hash table: пара (класс . название) -> форма")

(def-view-class объект-бд ()
  ((id :accessor id :db-kind :key :type integer)
   (model :accessor model
          :db-kind :virtual
          :allocation :class)) ;; модель для списка
  (:base-table catalog))

(defun тип-ссылки (тип)  ;; NB: все ссылочные типы
  "истина, если тип ссылочный"
  (eq тип :справочник))

(defun translit (c)
  (case (char-downcase c)
    (#\а "a")
    (#\б "b")
    (#\в "v")
    (#\г "g")
    (#\д "d")
    (#\е "ye")
    (#\ё "yo")
    (#\ж "zh")
    (#\з "z")
    (#\и "i")
    (#\й "j")
    (#\к "k")
    (#\л "l")
    (#\м "m")
    (#\н "n")
    (#\о "o")
    (#\п "p")
    (#\р "r")
    (#\с "s")
    (#\т "t")
    (#\у "u")
    (#\ф "f")
    (#\х "h")
    (#\ц "ts")
    (#\ч "ch")
    (#\ш "sh")
    (#\щ "sch")
    (#\ъ "")
    (#\ы "y")
    (#\ь "")
    (#\э "e")
    (#\ю "yu")
    (#\я "ya")
    (t (string c))))

(defun поле-бд (поле-бух)
  (if (consp поле-бух) (second поле-бух)
    (apply #'concatenate
           `(string ,@(loop :for c :across (string поле-бух)
                            :collecting (translit c))))))

(defun тип-бд (тип доп)
  ((lambda (l)
    (if (second l) l (car l)))
   (case тип
     ((:string :строка) (list 'varchar доп))
     ((:integer :целое) (list 'integer доп))
     ((:number :numeric :число) (cons 'number доп))
     ((:date :дата) (list 'date доп)) 
     (t (list тип доп)))))

(defun имя-лисп (имя &optional package)
  "параметр может быть уже именем (строкой или символом),
а может быть двухэлементным списком (имя-лисп имя-бд)"
  (let ((symbol (if (consp имя)
                    (car имя) имя)))
    (if package
        (intern (string symbol) package)
        symbol)))
        
(defun поля-бд (поля)
  "Эта функция на входе получает поля вида
 ((клиент :справочник клиенты)
 (дата :дата)
 (комментарий :строка 50)), а на выходе должна выдать поля бд вида
 ((клиент
   :accessor клиент
   :db-kind :join
   :db-info (:join-class клиенты
             :home-key client
             :foreign-key id
             :set nil))
  (дата
    :accessor дата
    :type :date
    :column data)
  (комментарий
    :accessor комментарий
    :type (varchar 50)
    :column kommentariy))

 имя поля может быть двухэлементным списком, например,
 ((клиент client) :справочник клиенты)
 в этом случае первое поле -- accessor, второе -- поле бд"
  (mapcar (lambda (поле)
            (apply
             (lambda (имя тип &optional доп)
               `(,(имя-лисп имя) :accessor ,(имя-лисп имя)
                 ,@(if (тип-ссылки тип)
                       `(:db-kind :join
                         :db-info (:join-class ,доп
                                   :home-key ,(поле-бд имя)
                                   :foreign-key "ID"
                                   :set nil))
                       `(:type ,(тип-бд тип доп)
                         :column ,(поле-бд имя))))) поле))
          поля))

(defun тип-gtk (тип)
  (case тип
    ((:string :строка) :string)
    ((:integer :целое) :int)
    ((:number :numeric :число) :int)
    ((:справочник) :int) 
    (t тип)))

(defmethod обновить-модель ((объект объект-бд))
  (let ((поля-объекта (gethash (class-of объект) *поля*)))
    (unless (and (slot-boundp объект 'model) (model объект))
      (setf (model объект)
            (make-instance 'list-store
                           :columns (mapcar (lambda (x) (тип-gtk (second x))) поля-объекта))))

    (clear (model объект))
    (let ((accessors (mapcar #'car поля-объекта)))
      (mapc (lambda (x)
              (append-values (model объект)
                             (mapcar (lambda (accessor)
                                       (funcall accessor (car x)))
                                     accessors)))
            (select (type-of объект))))))

(in-package :учёт)

(defgeneric форма-списка-по-объекту (объект))

(defun форма-списка (class-type)
  (форма-списка-по-объекту (make-instance class-type)))