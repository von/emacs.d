
(autoload 'todo-mode "todo-mode"
  "Major mode for editing TODO lists." t)

(autoload 'todo-show "todo-mode"
  "Show TODO items." t)

(autoload 'todo-insert-item "todo-mode"
  "Add TODO item." t)

(global-set-key "\C-ct" 'todo-show) ;; switch to TODO buffer
(global-set-key "\C-ci" 'todo-insert-item) ;; insert new item
