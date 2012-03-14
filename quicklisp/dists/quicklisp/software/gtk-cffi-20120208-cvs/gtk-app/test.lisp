(asdf:oos 'asdf:load-op :clsql)
(asdf:oos 'asdf:load-op :clsql-postgresql-socket)
(asdf:oos 'asdf:load-op :учёт)

(in-package :учёт)
(gtk-init)
(connect '("localhost" "monk" "monk" ""))
(справочник сотрудники ())

(defvar *w* (форма-списка 'справочник:сотрудники))

(show *w*)
(gtk-main)
