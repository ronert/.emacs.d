;; Bookmark +
(use-package bookmark+
  :bind
  ("<f7>" . bookmark-jump)
  ("<f8>" . bookmark-set)
  ("<f9>" . bookmark-bmenu-list)
)

(provide 'init-bookmarks)
