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
         (let* ((bg-color (gray 1.0))
                (fg-feint (gray 0.9))
                ;; Dark target in gradients
                (tuna (alter-color (gray 0.2)
                                   (set-hue 270)))
                ;; For neutral text
                (mine-shaft (gray 0.25))
                (concrete (gray 0.95))
                ;; Highlighted text, literal values, constants, quotes, etc.
                (chateu-green "#3ca355")
                (chateu-green-lt (alter-color chateu-green
                                   (set-lightness 70.0)
                                   (set-chroma 55.0)))
                (chateu-green-bg-1 (alter-color chateu-green
                                     (set-lightness 95.0)
                                     (set-chroma 20.0)))
                (chateu-green-bg-2 (alter-color chateu-green
                                     (set-lightness 85.0)
                                     (set-chroma 40.0)))
                ;; Keywords, important language bits, prompts, user interface
                ;; hilights etc.
                (pacific-blue "#00a0c1")
                (pacific-blue-lt (alter-color pacific-blue
                                   (set-lightness 70.0)
                                   (set-chroma 55.0)))
                (pacific-blue-bg-1 (alter-color pacific-blue
                                     (set-lightness 95.0)
                                     (set-chroma 20.0)))
                (pacific-blue-bg-2 (alter-color pacific-blue
                                     (set-lightness 85.0)
                                     (set-chroma 40.0)))
                ;; For names of stuff at the point of definition.
                (alpine "#b38a2c")
                (alpine-lt (alter-color alpine
                             (set-lightness 70.0)
                             (set-chroma 55.0)))
                (alpine-bg-1 (alter-color alpine
                               (set-lightness 95.0)
                               (set-chroma 20.0)))
                (alpine-bg-2 (alter-color alpine
                               (set-lightness 85.0)
                               (set-chroma 40.0)))
                (alpine-dark (alter-color alpine
                               (blend-to mine-shaft 0.6)))
                ;; Rarely used color; mainly negative diffs, redactions etc.
                (japonica "#da7176")
                (japonica-lt (alter-color japonica
                               (set-lightness 70.0)
                               (set-chroma 55.0)))
                (japonica-bg-1 (alter-color japonica
                                 (set-lightness 95.0)
                                 (set-chroma 20.0)))
                (japonica-bg-2 (alter-color japonica
                                 (set-lightness 85.0)
                                 (set-chroma 40.0)))
                ;; The primary attention color
                (bright-red "#ff0000")
                ;; The secondary attention color
                (yellow "yellow")
                ;; Inactive background elements
                (passive-background (gray 0.1))
                ;; Line numbers and stuff like that
                (passive-foreground (gray 0.7))
                ;; There shouldn't be any of these
                (undefined "violet"))

           (desc
            (bg bg-color
                default
                linum)

            (fg mine-shaft
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
            (fg pacific-blue link)
            (fg alpine link-visited)

            (attrs (:background (gray 0.95)
                    :foreground nil)
                   hl-line)


            ;; Mode line and prompt

            (fg alpine-bg-1
                mode-line)

            (bg alpine-bg-2
                mode-line-inactive)
            (fg alpine
                mode-line-inactive)

            (fg alpine-bg-2
                vertical-border)

            (bg mine-shaft
                mode-line)

            (attrs (:background nil
                    :foreground bg-color)
                   mode-line-buffer-id)

            (fg pacific-blue
                minibuffer-prompt
                completions-first-difference)

            (bg alpine-bg-1
                region)

            ;; Isearch & parentheses matching

            (attrs (:foreground mine-shaft
                    :background alpine-bg-2)
                   show-paren-match-face)

            (attrs (:foreground mine-shaft
                    :background alpine-bg-2)
                   isearch)

            (attrs (:foreground nil
                    :background alpine-bg-1)
                   lazy-highlight)

            (attrs (:foreground bg-color
                    :background bright-red)
                   isearch-fail
                   show-paren-mismatch-face)

            ;; Outline

            (fg mine-shaft
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

            (fg pacific-blue
                font-lock-builtin-face
                font-lock-keyword-face
                font-lock-type-face)

            (fg alpine
                font-lock-function-name-face
                font-lock-variable-name-face)

            (fg bright-red
                font-lock-preprocessor-face
                font-lock-warning-face)

            ;; Dired

            (fg pacific-blue
                dired-directory
                dired-symlink)

            (fg chateu-green
                dired-flagged
                dired-mark
                dired-marked)

            (fg mine-shaft
                dired-header)

            (fg passive-foreground
                dired-ignored
                dired-warning)

            ;; Compilation

            (fg pacific-blue
                compilation-column-number
                compilation-line-number)

            (fg bright-red
                compilation-error
                compilation-warning)

            (fg chateu-green
                compilation-info)

            ;; Markdown

            (fg pacific-blue
                markdown-comment-face)
            (fg mine-shaft
                markdown-header-face
                markdown-header-face-1
                markdown-header-face-2
                markdown-header-face-3
                markdown-header-face-4
                markdown-header-face-5
                markdown-header-face-6
                markdown-list-face
                markdown-math-face)
            (fg pacific-blue
                markdown-bold-face
                markdown-italic-face)
            (fg chateu-green
                markdown-inline-code-face
                markdown-pre-face
                markdown-blockquote-face)
            (fg alpine
                markdown-link-face
                markdown-link-title-face
                markdown-url-face
                markdown-reference-face)
            (fg bright-red
                markdown-missing-link-face)

            ;; Magit

            (attrs (:foreground mine-shaft
                    :background alpine-bg-1)
                   magit-diff-file-heading
                   magit-diff-hunk-heading)

            (attrs (:foreground mine-shaft
                    :background alpine-bg-2)
                   magit-diff-file-heading-highlight
                   magit-diff-hunk-heading-highlight)

            (bg chateu-green-bg-1 magit-diff-added)
            (bg chateu-green-bg-2 magit-diff-added-highlight)
            (bg japonica-bg-1 magit-diff-removed)
            (bg japonica-bg-2 magit-diff-removed-highlight)
            (bg nil magit-diff-context)
            (bg concrete
                magit-diff-context-highlight
                magit-section-highlight)

            (attrs (:background nil
                    :border nil)
                   magit-branch-local
                   magit-branch-remote
                   magit-branch-current)
            (fg alpine
                magit-branch-current)
            (fg alpine-lt
                magit-branch-local)
            (fg chateu-green
                magit-branch-remote)

            (fg pacific-blue magit-hash)

            (fg mine-shaft
                magit-log-author
                magit-log-date
                magit-log-graph)

            ))))
