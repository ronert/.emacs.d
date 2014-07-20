;; Bookmark +
(require-package 'bookmark+)
(global-set-key (kbd "<f7>") 'bookmark-jump)
(global-set-key (kbd "<f8>") 'bookmark-set)
(global-set-key (kbd "<f9>") 'bookmark-bmenu-list)

(provide 'init-bookmarks)
