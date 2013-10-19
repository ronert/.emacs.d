;; Bookmark +
(require-package 'bookmark+)
(global-set-key (kbd "<f7>") 'bookmark-jump)
(global-set-key (kbd "<f8>") 'bookmark-bmenu-list)
(global-set-key (kbd "<f9>") 'bookmark-set)

(provide 'init-bookmarks)
