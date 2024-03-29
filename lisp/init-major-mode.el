;;; init-major-mode.el --- init some major mode config -*- lexical-binding: t; -*-

;;; Code:

(use-package thrift-mode
  :mode "\\.thrift\\'"
  :hook (thrift-mode . display-line-numbers-mode))

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :hook (protobuf-mode . display-line-numbers-mode))

(use-package json-mode
  :mode "\\.json\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package yaml-mode
  :mode "\\.\\(yml\\|yaml\\)\\'")

(use-package graphql-mode
  :mode "\\.\\(gql\\|graphql\\)\\'")

(provide 'init-major-mode)
;;; init-major-mode.el ends here
