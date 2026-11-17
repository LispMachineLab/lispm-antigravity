;;; lispm-antigravity-test.el --- Tests for lispm-antigravity  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-antigravity
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run with `make test', lispm-agent's lisp/ on the load path.  They need
;; neither Antigravity nor its store: the store is made up when read.

;;; Code:

(require 'ert)
(require 'lispm-antigravity)

(ert-deftest lispm-antigravity-test-defined-on-load ()
  "Loaded, Antigravity is one of the editor's agents, taking part while its
command is on PATH, started with y."
  (let ((props (alist-get 'antigravity lispm-agents-defined)))
    (should (equal (plist-get props :name) "Antigravity"))
    (should (equal (plist-get props :program) "agy"))
    (should (equal (plist-get props :key) "y"))
    (should (equal (plist-get props :text-icon) "✦"))
    (should-not (plist-get props :built-in))
    (should (file-readable-p (nth 1 (plist-get props :icon))))
    (should (equal (funcall (plist-get props :resume-arguments) "abc-123")
                   '("--conversation" "abc-123")))
    (should (equal (funcall (plist-get props :model-arguments) "gemini-4-pro")
                   '("--model" "gemini-4-pro"))))
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (should-not (lispm-agents-available-p 'antigravity)))
  (cl-letf (((symbol-function 'executable-find) (lambda (program &rest _) (concat "/bin/" program))))
    (should (lispm-agents-available-p 'antigravity))
    (should (eq (keymap-lookup lispm-agents-map "y") 'lispm-antigravity-new))))

(ert-deftest lispm-antigravity-test-sessions-from-rows-and-files ()
  "A row of the store is a session titled by its annotation, summary title,
preview, worktree role, or first transcript prompt; subagents carry :parent
and are not resumable on their own."
  (let* ((lispm-antigravity-state-directory (make-temp-file "lispm-antigravity-test" t))
         (lispm-antigravity--head-cache (make-hash-table :test #'equal))
         (id1 "11111111-2222-3333-4444-555555555555")
         (id2 "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
         (id3 "99999999-8888-7777-6666-555555555555")
         (log2 (expand-file-name (format "brain/%s/.system_generated/logs/transcript.jsonl" id2)
                                 lispm-antigravity-state-directory)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "annotations" lispm-antigravity-state-directory) t)
          (with-temp-file (lispm-antigravity--annotation-file id1)
            (insert "title:\"Custom Annotation Title\"  last_user_view_time:{seconds:100}\n"))
          (make-directory (file-name-directory log2) t)
          (with-temp-file log2
            (insert "{\"step_index\":0,\"source\":\"USER_EXPLICIT\",\"type\":\"USER_INPUT\","
                    "\"status\":\"DONE\",\"created_at\":\"2026-11-17T05:00:00Z\","
                    "\"content\":\"<USER_REQUEST>\\nFix the parser bug\\n</USER_REQUEST>\"}\n"))
          (let ((s1 (lispm-antigravity--session
                     (list id1 "Summary Title" "Preview" "2026-11-17 06:00:00+00:00"
                           "[\"file:///src/project\"]" "" "" "2026-11-17 05:30:00+00:00")))
                (s2 (lispm-antigravity--session
                     (list id2 "" "" "2026-11-17 06:00:00+00:00"
                           "[]" "" "" "0001-01-01 00:00:00+00:00")))
                (s3 (lispm-antigravity--session
                     (list id3 "" "" "2026-11-17 06:10:00+00:00"
                           (format "[\"file://%s/worktrees/%s/subagent-Database-Auditor-self-1a2b3c4d\"]"
                                   lispm-antigravity-state-directory id1)
                           id1 "self" "0001-01-01 00:00:00+00:00"))))
            (should (equal (plist-get s1 :agent) 'antigravity))
            (should (equal (plist-get s1 :id) id1))
            (should (equal (plist-get s1 :cwd) "/src/project"))
            (should (equal (plist-get s1 :title) "Custom Annotation Title"))
            (should (lispm-antigravity--resumable-p s1))
            (should (equal (plist-get s2 :cwd) (expand-file-name "~")))
            (should (equal (plist-get s2 :title) "Fix the parser bug"))
            (should (equal (plist-get s2 :file) log2))
            (should (= (plist-get s2 :started)
                       (float-time (parse-iso8601-time-string "2026-11-17T05:00:00Z"))))
            (should (equal (plist-get s3 :parent) id1))
            (should (equal (plist-get s3 :title) "Database Auditor"))
            (should-not (lispm-antigravity--resumable-p s3))))
      (delete-directory lispm-antigravity-state-directory t))))

(ert-deftest lispm-antigravity-test-worktree ()
  "An Antigravity worktree resolves its label and joins its parent's project."
  (let* ((lispm-antigravity-state-directory "/tmp/agy-state/")
         (lispm-antigravity--cache
          '(nil (:agent antigravity :id "parent-1" :cwd "/src/repo" :title "Main")))
         (wt (lispm-antigravity--worktree "/tmp/agy-state/worktrees/parent-1/subagent-Fix-123")))
    (should (equal (plist-get wt :root) "/src/repo"))
    (should (equal (plist-get wt :label) "⎇ subagent-Fix-123"))))

(ert-deftest lispm-antigravity-test-rename-and-delete ()
  "Renaming updates both the annotation file and the SQLite summary store;
deleting removes the summary row and the session's files."
  (skip-unless (sqlite-available-p))
  (let* ((lispm-antigravity-state-directory (make-temp-file "lispm-antigravity-test" t))
         (state-file (lispm-antigravity--state-file))
         (id "4e8a4b99-7a88-4ca4-9c8b-15d08659c1e6")
         (db (sqlite-open state-file)))
    (unwind-protect
        (progn
          (sqlite-execute db "CREATE TABLE conversation_summaries (
                                conversation_id TEXT PRIMARY KEY,
                                title TEXT, preview TEXT, step_count INTEGER,
                                last_modified_time TEXT, workspace_uris TEXT,
                                parent_conversation_id TEXT, agent_name TEXT,
                                status TEXT, not_fully_idle INTEGER,
                                last_user_input_time TEXT)")
          (sqlite-execute db "INSERT INTO conversation_summaries VALUES
                                (?, 'Old Title', 'Old Preview', 5, '2026-11-17 07:00:00+00:00',
                                 '[\"file:///src/app\"]', '', '', 'CASCADE_RUN_STATUS_IDLE', 0,
                                 '2026-11-17 06:50:00+00:00')"
                          (list id))
          (sqlite-close db)
          (setq db nil)
          (make-directory (expand-file-name (format "brain/%s" id) lispm-antigravity-state-directory) t)
          (let ((sessions (lispm-antigravity-sessions)))
            (should (= (length sessions) 1))
            (should (equal (plist-get (car sessions) :title) "Old Title"))
            (lispm-antigravity--rename (car sessions) "New \"Quoted\" Title")
            (should (equal (lispm-antigravity--annotation-title id) "New \"Quoted\" Title"))
            (should (equal (plist-get (car (lispm-antigravity-sessions)) :title)
                           "New \"Quoted\" Title"))
            (lispm-antigravity--delete (car sessions))
            (should-not (lispm-antigravity-sessions))
            (should-not (file-exists-p (lispm-antigravity--annotation-file id)))
            (should-not (file-exists-p (expand-file-name (format "brain/%s" id)
                                                         lispm-antigravity-state-directory)))))
      (when db (sqlite-close db))
      (delete-directory lispm-antigravity-state-directory t))))

(ert-deftest lispm-antigravity-test-ready-and-work-details ()
  "Antigravity waits for its user when its latest turn ended with a final
PLANNER_RESPONSE calling no tools, and reports pending/recent tool calls."
  (let* ((lispm-antigravity-state-directory (make-temp-file "lispm-antigravity-test" t))
         (id "session-ready-1")
         (log (expand-file-name (format "brain/%s/.system_generated/logs/transcript.jsonl" id)
                                lispm-antigravity-state-directory))
         (terminal (generate-new-buffer " *antigravity:test*")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory log) t)
          (with-current-buffer terminal
            (setq lispm-agents-buffer-agent 'antigravity
                  lispm-agents-buffer-session-id id))
          (with-temp-file log
            (insert "{\"step_index\":0,\"type\":\"USER_INPUT\",\"status\":\"DONE\","
                    "\"created_at\":\"2026-11-17T07:00:00Z\",\"content\":\"hi\"}\n"
                    "{\"step_index\":1,\"type\":\"PLANNER_RESPONSE\",\"status\":\"DONE\","
                    "\"created_at\":\"2026-11-17T07:00:01Z\","
                    "\"tool_calls\":[{\"name\":\"read_file\",\"args\":{\"Path\":\"/a\"}}]}\n"))
          (should-not (lispm-antigravity--ready-p terminal))
          (let ((work (lispm-antigravity--work-details terminal)))
            (should (equal (plist-get work :turn) "0"))
            (should-not (plist-get work :turn-ended))
            (should (= (length (plist-get work :pending-calls)) 1))
            (should (equal (plist-get (car (plist-get work :pending-calls)) :name) "read_file")))
          (with-temp-file log
            (insert "{\"step_index\":0,\"type\":\"USER_INPUT\",\"status\":\"DONE\","
                    "\"created_at\":\"2026-11-17T07:00:00Z\",\"content\":\"hi\"}\n"
                    "{\"step_index\":1,\"type\":\"PLANNER_RESPONSE\",\"status\":\"DONE\","
                    "\"created_at\":\"2026-11-17T07:00:01Z\","
                    "\"tool_calls\":[{\"name\":\"read_file\",\"args\":{\"Path\":\"/a\"}}]}\n"
                    "{\"step_index\":2,\"type\":\"GENERIC\",\"status\":\"DONE\","
                    "\"created_at\":\"2026-11-17T07:00:02Z\",\"content\":\"ok\"}\n"
                    "{\"step_index\":3,\"type\":\"PLANNER_RESPONSE\",\"status\":\"DONE\","
                    "\"created_at\":\"2026-11-17T07:00:03Z\",\"content\":\"Done.\"}\n"))
          (clrhash lispm-agents--work-cache)
          (should (lispm-antigravity--ready-p terminal))
          (let ((work (lispm-antigravity--work-details terminal)))
            (should (plist-get work :turn-ended))
            (should-not (plist-get work :pending-calls))
            (should (= (length (plist-get work :recent-calls)) 1))
            (should (eq (plist-get (car (plist-get work :recent-calls)) :status) 'done))))
      (kill-buffer terminal)
      (delete-directory lispm-antigravity-state-directory t))))

(ert-deftest lispm-antigravity-test-configured-and-setup ()
  "Antigravity's `mcp_config.json' names this package's bridge, another, or none."
  (should (file-executable-p lispm-antigravity-bridge))
  (should (equal (lispm-antigravity-setup-arguments)
                 (list "mcp" "add" "emacs" lispm-antigravity-bridge)))
  (let* ((directory (make-temp-file "lispm-antigravity-test" t))
         (lispm-antigravity-config-file (expand-file-name "mcp_config.json" directory)))
    (unwind-protect
        (cl-flet ((config (text) (with-temp-file lispm-antigravity-config-file (insert text))))
          (should-not (lispm-antigravity-configured))
          (config "")
          (should-not (lispm-antigravity-configured))
          (config "{\"mcpServers\":{}}")
          (should-not (lispm-antigravity-configured))
          (config (json-serialize
                   `((mcpServers . ((emacs . ((command . ,lispm-antigravity-bridge)
                                              (disabled . :false))))))))
          (should (eq (lispm-antigravity-configured) 'this))
          (config (json-serialize
                   `((mcpServers . ((emacs . ((command . ,lispm-antigravity-bridge)
                                              (disabled . t))))))))
          (should-not (lispm-antigravity-configured))
          (config "{\"mcpServers\":{\"emacs\":{\"command\":\"/old/bin/emacs-mcp\"}}}")
          (should (eq (lispm-antigravity-configured) 'other)))
      (delete-directory directory t))))

(ert-deftest lispm-antigravity-test-server-token-file-and-unload ()
  "An Antigravity terminal's server hands the bridge its address and a token
file only its owner can read, removed when the server stops or the package
is unloaded."
  (let* ((lispm-antigravity-token-directory (make-temp-file "lispm-antigravity-tokens" t))
         (lispm-antigravity-config-file "/nonexistent/mcp_config.json")
         (inhibit-message t)
         (terminal (get-buffer-create " *lispm-antigravity-test terminal*"))
         (server (lispm-antigravity--context temporary-file-directory))
         (file (lispm-antigravity--token-file server)))
    (unwind-protect
        (progn
          (should (lispm-antigravity-mcp-p server))
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         (lispm-agents-mcp-token server)))
          (should (= (file-modes file) #o600))
          (should (member (format "LISPM_ANTIGRAVITY_MCP_TOKEN_FILE=%s" file)
                          (lispm-agents-context-environment server)))
          (with-current-buffer terminal (setq lispm-agents-buffer-agent 'antigravity))
          (lispm-agents-context-bind server terminal)
          (unload-feature 'lispm-antigravity t)
          (should-not (assq 'antigravity lispm-agents-defined))
          (should-not (process-live-p (lispm-agents-mcp-server server)))
          (should-not (file-exists-p file))
          (should (buffer-live-p terminal)))
      (require 'lispm-antigravity)
      (kill-buffer terminal)
      (delete-directory lispm-antigravity-token-directory t)))
  (should (assq 'antigravity lispm-agents-defined)))

(ert-deftest lispm-antigravity-test-knows-no-desktop ()
  "Antigravity's extension stands alone beside lispm-agent: none of its files
but the tests names LispM Desktop's code."
  (dolist (file (directory-files (file-name-directory (locate-library "lispm-antigravity")) t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (and (re-search-forward "lispm-desktop\\|platinum" nil t)
                       (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos)))))))

(provide 'lispm-antigravity-test)
;;; lispm-antigravity-test.el ends here
