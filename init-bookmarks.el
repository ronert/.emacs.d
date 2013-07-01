;; Bookmark +
(require-package 'bookmark+)
(global-set-key (kbd "<f13>") 'bookmark-set)
(global-set-key (kbd "<f14>") 'bookmark-bmenu-list)
(global-set-key (kbd "<f15>") 'bookmark-jump)

(provide 'init-bookmarks)
