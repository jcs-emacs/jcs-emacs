;;; jcs-typescript-func.el --- TypeScript related.  -*- lexical-binding: t -*-
;;; Commentary: When editing the TypeScript related file.
;;; Code:


;;;###autoload
(defun jcs-typescript-ask-source (sc)
  "Ask the source SC for editing TypeScript file."
  (interactive
   (list (completing-read
          "Major source for this TypeScript file: " '("Default"
                                                      "Cocos Creator Scripting"))))
  (cond ((string= sc "Default") (jcs-insert-typescript-template))
        ((string= sc "Cocos Creator Scripting") (jcs-insert-typescript-cocos-creator-template))))


(provide 'jcs-typescript-func)
;;; jcs-typescript-func.el ends here
