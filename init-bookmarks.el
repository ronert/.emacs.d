;; Bookmark +
(require-package 'bookmark+)
(global-set-key (kbd "<f13>") 'bookmark-jump)
(global-set-key (kbd "<f14>") 'bookmark-bmenu-list)
(global-set-key (kbd "<f15>") 'bookmark-set)

(provide 'init-bookmarks)
