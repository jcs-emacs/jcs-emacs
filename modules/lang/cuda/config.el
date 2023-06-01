;;; lang/cuda/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-cuda-template "cuda" "default.txt"
  "Template for CUDA.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'cuda-mode-hook
  (run-hooks 'prog-mode-hook)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cu")
                              'jcs-insert-cuda-template))
