;;; vanilla-theme.el -- a light high-contract color theme for Emacs
;;; Copyright (C) 2012-15 Matti Hänninen <matti@mjhanninen.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; NOTE: This theme is currently in sketching phase and, thus, badly
;;; work-in-progress. For instance, this doesn't define an actual theme, this
;;; just applies the faces.

(require 'liquorice)

(apply #'custom-set-faces
       (liquorice-build-face-specs
         (let ((bg-color (gray 1.0))
               (fg-strong (gray 0.0))
               (fg-feint (gray 0.9))
               ;; For neutral text
               (celeste-gray "#d5d7d0")
               ;; Highlighted text
               ;; Literal values, constants, quotes, etc.
               (chateu-green "#43b25e")
               ;; Keywords, prompts, user interface hilights etc.
               (shakespeare-blue "#4f9db0")
               ;; For names of stuff at the point of definition.
               (tussock-brown "#b99246")
               ;; The primary attention color
               (bright-red "#ff0000")
               ;; The secondary attention color
               (yellow "yellow")
               ;; Inactive background elements
               (passive-background (gray 0.1))
               ;; Line numbers and stuff like that
               (passive-foreground (gray 0.7))
               ;; There shouldn't be any of these
               (undefined "violet")
               (dark-red (rgb 0.25 0.0 0.0)))
           (desc
            (bg bg-color
                default
                linum)

            (fg fg-strong
                default)

            (attrs (:weight 'normal
                    :slant 'normal
                    :underline nil
                    :overline nil
                    :strike-through nil
                    :box nil
                    :inherit nil)
                   linum)
            (fg fg-feint
                linum)

            (bg passive-background
                fringe)

            (fg bright-red
                warning)

            (attrs (:underline t)
                   link
                   link-visited)
            (fg shakespeare-blue link)
            (fg tussock-brown link-visited)

            (attrs (:background (gray 0.95)
                    :foreground nil)
                   hl-line)

            (fg celeste-gray
                mode-line
                mode-line-inactive)

            ;; Lets emphasise the buffer ID a bit

            (attrs (:background nil
                    :foreground yellow)
                   mode-line-buffer-id)

            (fg shakespeare-blue
                minibuffer-prompt
                completions-first-difference)

            (bg "navy"
                region)

            ;; TODO: Rethink this comment

            ;; Many of the emphasis things have black typeface on color
            ;; coded background with the yellow signalling something is
            ;; oddly wrong.

            (fg fg-strong
                isearch-fail
                lazy-highlight
                show-paren-match-face
                show-paren-mismatch-face)

            (bg shakespeare-blue
                lazy-highlight
                show-paren-match-face)

            (bg yellow
                isearch-fail
                show-paren-mismatch-face)

            ;; Except isearch is special

            (attrs (:foreground bg-color
                    :background bright-red)
                   isearch)

            ;; Outline

            (fg fg-strong
                outline-1
                outline-2
                outline-3
                outline-4
                outline-5
                outline-6
                outline-7
                outline-8)

            ;; Font-lock

            (fg passive-foreground
                font-lock-comment-face
                font-lock-comment-delimiter-face
                font-lock-doc-face
                font-lock-doc-string-face)

            (fg chateu-green
                font-lock-string-face
                font-lock-regexp-grouping-backslash
                font-lock-regexp-grouping-construct
                font-lock-constant-face)

            (fg shakespeare-blue
                font-lock-builtin-face
                font-lock-keyword-face
                font-lock-type-face)

            (fg tussock-brown
                font-lock-function-name-face
                font-lock-variable-name-face)

            (fg bright-red
                font-lock-preprocessor-face
                font-lock-warning-face)

            ;; Dired

            (fg shakespeare-blue
                dired-directory
                dired-symlink)

            (fg chateu-green
                dired-flagged
                dired-mark
                dired-marked)

            (fg fg-strong
                dired-header)

            (fg passive-foreground
                dired-ignored
                dired-warning)

            ;; Compilation

            (fg shakespeare-blue
                compilation-column-number
                compilation-line-number)

            (fg bright-red
                compilation-error
                compilation-warning)

            (fg chateu-green
                compilation-info)

            ;; Markdown

            (fg shakespeare-blue
                markdown-blockquote-face
                markdown-comment-face)
            (fg fg-strong
                markdown-header-face
                markdown-header-face-1
                markdown-header-face-2
                markdown-header-face-3
                markdown-header-face-4
                markdown-header-face-5
                markdown-header-face-6)
            (fg fg-strong
                markdown-bold-face
                markdown-italic-face)
            (fg chateu-green
                markdown-inline-code-face
                markdown-pre-face)
            (fg tussock-brown
                markdown-link-face
                markdown-link-title-face
                markdown-url-face
                markdown-reference-face)
            (fg celeste-gray
                markdown-list-face
                markdown-math-face)
            (fg bright-red
                markdown-missing-link-face)))))
