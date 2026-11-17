;;; lispm-antigravity.el --- Antigravity, one of the editor's agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-antigravity
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, processes
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

;; Google Antigravity (`agy'), as one of the editor's agents of LispM
;; Agent (lispm-agents.el), beside Claude Code and Codex: a package of its
;; own, which LispM Desktop's App Store installs.  Loading this file makes
;; Antigravity an agent (`lispm-agents-define'); unloading it, as the App
;; Store does when it removes it, takes it away again.  It takes part while
;; `agy' is on PATH: y in `lispm-agents-map' starts it in the current
;; project, and its sessions join the others.
;;
;; It needs lispm-agent 0.8.0 or later (the package `lispm'), which LispM
;; Desktop loads before it.  Package-Requires leaves it out on purpose:
;; package.el would not activate this package where lispm-agent is a
;; checkout on the load path rather than a package, or a package of an
;; older version, and it could not be loaded at all.
;;
;; - Its sessions are read from Antigravity's SQLite summary store,
;;   ~/.gemini/antigravity/conversation_summaries.db, titled by the custom
;;   title in ~/.gemini/antigravity/annotations/ID.pbtxt when one was
;;   chosen, else the summary title or preview, else the first prompt in
;;   ~/.gemini/antigravity/brain/ID/.system_generated/logs/transcript.jsonl.
;;   Subagents record their parent conversation id (`:parent') and join
;;   their parent's session tree.
;; - It sees Emacs's state through an MCP server per terminal
;;   (lispm-agents-mcp.el).  Antigravity reads global MCP servers from
;;   ~/.gemini/config/mcp_config.json, where this package's stdio bridge,
;;   bin/emacs-mcp, is registered once as the server `emacs'
;;   (`lispm-antigravity-setup'); each terminal then hands the bridge its
;;   server's address and a token file:
;;
;;     agy, with LISPM_ANTIGRAVITY_MCP_URL and LISPM_ANTIGRAVITY_MCP_TOKEN_FILE
;;       --> bin/emacs-mcp --> POST /mcp with "Authorization: Bearer TOKEN"
;;
;;   The token is written to a file readable by its owner only, and only
;;   the file's name travels in the environment.
;; - It waits for its user's next message when its transcript's latest
;;   turn has ended (a completed `PLANNER_RESPONSE' calling no tools), or
;;   when its summary row is idle: other agents' messages then reach it
;;   (`lispm-antigravity--ready-p').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'parse-time)
(require 'url-util)
(require 'lispm-agents)
(require 'lispm-agents-mcp)

(declare-function lispm-agents-terminal-session-id "lispm-agents" (terminal))

(defconst lispm-antigravity-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of this package, holding its bridge and its icon.")

(defconst lispm-antigravity-program "agy"
  "Antigravity's command.")

(defvar lispm-antigravity-state-directory
  (expand-file-name "~/.gemini/antigravity/")
  "Directory where Antigravity keeps its store, transcripts and annotations.")

(defvar lispm-antigravity-config-file
  (expand-file-name "~/.gemini/config/mcp_config.json")
  "Antigravity's MCP configuration file.")

(defconst lispm-antigravity-bridge
  (expand-file-name "bin/emacs-mcp" lispm-antigravity-directory)
  "The stdio MCP server Antigravity runs, which forwards to a terminal's server.")

(defvar lispm-antigravity-token-directory
  (expand-file-name "lispm-antigravity/" temporary-file-directory)
  "Directory of the token files written for Antigravity terminals.
It lies under the user's own temporary directory, which others cannot
enter.")

(defconst lispm-antigravity-query
  "SELECT conversation_id, title, preview, last_modified_time, workspace_uris,
          parent_conversation_id, agent_name, last_user_input_time
     FROM conversation_summaries
    WHERE step_count > 0"
  "Query listing Antigravity conversations with at least one recorded step.")

(defun lispm-antigravity--state-file ()
  "Return the path to Antigravity's SQLite summary database."
  (expand-file-name "conversation_summaries.db" lispm-antigravity-state-directory))

(defun lispm-antigravity--annotation-file (id)
  "Return the annotation `.pbtxt' file for conversation ID."
  (expand-file-name (format "annotations/%s.pbtxt" id)
                    lispm-antigravity-state-directory))

(defun lispm-antigravity--transcript-file (id)
  "Return the transcript `.jsonl' file of conversation ID when it exists."
  (let* ((logs (expand-file-name (format "brain/%s/.system_generated/logs/" id)
                                 lispm-antigravity-state-directory))
         (compact (expand-file-name "transcript.jsonl" logs))
         (full (expand-file-name "transcript_full.jsonl" logs)))
    (cond ((file-readable-p compact) compact)
          ((file-readable-p full) full))))

;;;; Sessions (pure & helpers)

(defun lispm-antigravity--non-blank (value)
  "Return VALUE trimmed when it is a non-blank string, else nil."
  (when (and (stringp value) (not (string-blank-p value)))
    (string-trim value)))

(defun lispm-antigravity--parse-time (value)
  "Return float seconds for timestamp VALUE, or nil when missing.
Antigravity writes SQLite timestamps with a space before the clock, which
is normalized to `T' so `parse-iso8601-time-string' honors the zone."
  (cond ((and (numberp value) (> value 0)) (float value))
        ((and (stringp value) (not (string-blank-p value)))
         (when-let* ((iso (subst-char-in-string ?\s ?T (string-trim value)))
                     (parsed (ignore-errors (parse-iso8601-time-string iso)))
                     (seconds (ignore-errors (float-time parsed)))
                     ((> seconds 0)))
           seconds))))

(defun lispm-antigravity--workspace-uri (uris)
  "Return the first workspace URI in URIS, a JSON array string, or nil."
  (when (and (stringp uris) (string-prefix-p "[" (string-trim-left uris)))
    (car (ignore-errors
           (json-parse-string uris :array-type 'list
                              :null-object nil :false-object nil)))))

(defun lispm-antigravity--uri-directory (uri)
  "Return the local directory path for `file://' URI, or nil."
  (when (and (stringp uri) (string-prefix-p "file://" uri))
    (let ((path (url-unhex-string (substring uri (length "file://")))))
      (and (not (string-empty-p path)) path))))

(defun lispm-antigravity--workspace-directory (uris)
  "Return the working directory from URIS, else the user's home directory."
  (or (lispm-antigravity--uri-directory (lispm-antigravity--workspace-uri uris))
      (expand-file-name "~")))

(defun lispm-antigravity--unescape-pbtxt (text)
  "Unescape `\\\"' and `\\\\' in protobuf text string TEXT."
  (replace-regexp-in-string
   "\\\\\\([\"\\\\nrt]\\)"
   (lambda (match)
     (pcase (aref match 1)
       (?n "\n")
       (?r "\r")
       (?t "\t")
       (char (string char))))
   text t t))

(defun lispm-antigravity--escape-pbtxt (text)
  "Escape TEXT for a double-quoted protobuf text string literal."
  (let ((single-line (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))))
    (replace-regexp-in-string "\\([\"\\\\]\\)" "\\\\\\1" single-line t)))

(defun lispm-antigravity--annotation-title (id)
  "Return the custom title recorded in the annotation file of ID, or nil."
  (let ((file (lispm-antigravity--annotation-file id)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "\\btitle:[ \t]*\"\\(\\(?:\\\\.\\|[^\"\\]\\)*\\)\"" nil t)
          (lispm-antigravity--non-blank
           (lispm-antigravity--unescape-pbtxt (match-string 1))))))))

(defun lispm-antigravity--user-prompt (content)
  "Return the human prompt in Antigravity `USER_INPUT' CONTENT, or nil.
Antigravity wraps user messages in `<USER_REQUEST>...</USER_REQUEST>' tags."
  (when (stringp content)
    (let ((text (if (and (string-match "<USER_REQUEST>[ \t\n\r]*\\(\\(?:.\\|\n\\)*?\\)[ \t\n\r]*</USER_REQUEST>"
                                       content))
                    (match-string 1 content)
                  content)))
      (and (lispm-agents-human-text-p text) (string-trim text)))))

(defun lispm-antigravity--subagent-worktree-title (cwd agent-name)
  "Return a human title from subagent worktree CWD and AGENT-NAME, or nil."
  (when (and (stringp cwd)
             (string-match "/worktrees/[^/]+/subagent-\\(.+\\)-[0-9a-f]\\{8\\}/?\\'" cwd))
    (let* ((body (match-string 1 cwd))
           (suffix (and (lispm-antigravity--non-blank agent-name)
                        (concat "-" (string-trim agent-name))))
           (role (if (and suffix (string-suffix-p suffix body))
                     (substring body 0 (- (length body) (length suffix)))
                   body))
           (clean (string-trim (replace-regexp-in-string "-+" " " role))))
      (lispm-antigravity--non-blank clean))))

(defun lispm-antigravity--system-task (content)
  "Return the first task line from a subagent `SYSTEM_MESSAGE' CONTENT."
  (when (and (stringp content)
             (string-match "content=\\(\\(?:.\\|\n\\)*?\\)\\(?:</SYSTEM_MESSAGE>\\|\\'\\)" content))
    (let ((lines (split-string (match-string 1 content) "\n" t "[ \t\r#*-]+")))
      (lispm-antigravity--non-blank (car lines)))))

(defvar lispm-antigravity--head-cache (make-hash-table :test #'equal)
  "Memoized (MTIME STARTED PROMPT) per transcript file.")

(defun lispm-antigravity--transcript-head-info (file &optional subagentp)
  "Return (STARTED . PROMPT) from the head of transcript FILE.
When SUBAGENTP is non-nil, PROMPT falls back to the initial system task."
  (when (and (stringp file) (file-readable-p file))
    (let* ((mtime (lispm-agents-file-time file))
           (key (cons file (and subagentp t)))
           (cached (gethash key lispm-antigravity--head-cache)))
      (if (and cached (equal (car cached) mtime))
          (cdr cached)
        (let* ((size (file-attribute-size (file-attributes file)))
               (objects (lispm-agents-read-json-region
                         file 0 (min size lispm-agents-transcript-head-bytes)))
               (started (seq-some (lambda (obj)
                                    (lispm-antigravity--parse-time (plist-get obj :created_at)))
                                  objects))
               (prompt (or (seq-some (lambda (obj)
                                       (and (equal (plist-get obj :type) "USER_INPUT")
                                            (lispm-antigravity--user-prompt (plist-get obj :content))))
                                     objects)
                           (and subagentp
                                (seq-some (lambda (obj)
                                            (and (equal (plist-get obj :type) "SYSTEM_MESSAGE")
                                                 (lispm-antigravity--system-task (plist-get obj :content))))
                                          objects))))
               (info (cons started prompt)))
          (puthash key (cons mtime info) lispm-antigravity--head-cache)
          info)))))

(defun lispm-antigravity--session (row)
  "Return the Antigravity session plist for ROW of `lispm-antigravity-query'.
Return nil when the session has neither a title nor a prompt."
  (pcase-let ((`(,id ,title ,preview ,modified ,uris ,parent ,agent-name ,input-time) row))
    (when (lispm-antigravity--non-blank id)
      (let* ((parent (lispm-antigravity--non-blank parent))
             (cwd (lispm-antigravity--workspace-directory uris))
             (file (lispm-antigravity--transcript-file id))
             (head nil)
             (head-info (lambda ()
                          (or head
                              (setq head (or (lispm-antigravity--transcript-head-info file parent)
                                             '(nil . nil))))))
             (title (or (lispm-antigravity--annotation-title id)
                        (lispm-antigravity--non-blank title)
                        (lispm-antigravity--non-blank preview)
                        (and parent (lispm-antigravity--subagent-worktree-title cwd agent-name))
                        (cdr (funcall head-info))
                        (and parent (lispm-antigravity--non-blank agent-name))))
             (mod-time (lispm-antigravity--parse-time modified))
             (file-time (and file (lispm-agents-file-time file)))
             (time (cond ((and mod-time file-time) (max mod-time file-time))
                         (mod-time mod-time)
                         (file-time file-time)))
             (started (or (lispm-antigravity--parse-time input-time)
                          (car (funcall head-info))
                          time)))
        (when (and title time)
          (append (list :agent 'antigravity
                        :id id
                        :cwd cwd
                        :title title
                        :time time
                        :started started)
                  (and file (list :file file))
                  (and parent (list :parent parent))))))))

;;;; Sessions (effects)

(defvar lispm-antigravity--cache nil
  "Memoized Antigravity sessions as (STAMP . SESSIONS).")

(defun lispm-antigravity--rows ()
  "Return the rows of `lispm-antigravity-query' from Antigravity's store.
Return nil without the store, or without SQLite in this Emacs."
  (let ((state-file (lispm-antigravity--state-file)))
    (when (and (sqlite-available-p) (file-readable-p state-file))
      (let ((db (sqlite-open state-file)))
        (unwind-protect
            (sqlite-select db lispm-antigravity-query)
          (sqlite-close db))))))

(defun lispm-antigravity--stamp ()
  "Return the modification times of Antigravity's store and annotations."
  (let ((state-file (lispm-antigravity--state-file))
        (annotations (expand-file-name "annotations" lispm-antigravity-state-directory)))
    (mapcar (lambda (file) (and (file-exists-p file) (lispm-agents-file-time file)))
            (list state-file (concat state-file "-wal") annotations))))

(defun lispm-antigravity-sessions ()
  "Return every recorded Antigravity session, rereading the store on change."
  (let ((stamp (lispm-antigravity--stamp)))
    (unless (and lispm-antigravity--cache (equal (car lispm-antigravity--cache) stamp))
      (setq lispm-antigravity--cache
            (cons stamp
                  (condition-case err
                      (delq nil (mapcar #'lispm-antigravity--session (lispm-antigravity--rows)))
                    (error (message "Antigravity sessions unavailable: %s" (error-message-string err))
                           nil)))))
    (cdr lispm-antigravity--cache)))

(defun lispm-antigravity--resumable-p (session)
  "Return non-nil when Antigravity can resume SESSION: a top-level session,
not a subagent's, which is opened as its transcript instead."
  (not (plist-get session :parent)))

(defun lispm-antigravity--worktree (directory)
  "Return where DIRECTORY belongs when it is in an Antigravity worktree.
Antigravity places worktrees under STATE-DIR/worktrees/PARENT-ID/NAME."
  (let ((root (directory-file-name (expand-file-name lispm-antigravity-state-directory)))
        (dir (directory-file-name (expand-file-name directory))))
    (when (string-match (concat "\\`" (regexp-quote root) "/worktrees/\\([^/]+\\)/\\([^/]+\\)")
                        dir)
      (let* ((parent-id (match-string 1 dir))
             (name (match-string 2 dir))
             (parent (and lispm-antigravity--cache
                          (seq-find (lambda (s) (equal (plist-get s :id) parent-id))
                                    (cdr lispm-antigravity--cache))))
             (parent-cwd (and parent (plist-get parent :cwd))))
        (append (when (and parent-cwd (not (string-prefix-p (concat root "/worktrees/") parent-cwd)))
                  (list :root parent-cwd))
                (list :label (concat "⎇ " name)))))))

(defun lispm-antigravity--rename (session title)
  "Give Antigravity SESSION the TITLE in its annotation file and summary store."
  (let* ((id (plist-get session :id))
         (file (lispm-antigravity--annotation-file id))
         (escaped (format "title:\"%s\"" (lispm-antigravity--escape-pbtxt title)))
         (coding-system-for-write 'utf-8-unix)
         (state-file (lispm-antigravity--state-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (when (file-readable-p file)
        (insert-file-contents file))
      (goto-char (point-min))
      (if (re-search-forward "\\btitle:[ \t]*\"\\(?:\\\\.\\|[^\"\\]\\)*\"" nil t)
          (replace-match escaped t t)
        (goto-char (point-min))
        (insert escaped (if (eobp) "\n" "  ")))
      (write-region (point-min) (point-max) file nil 'silent))
    (when (and (sqlite-available-p) (file-writable-p state-file))
      (let ((db (sqlite-open state-file)))
        (unwind-protect
            (progn
              (sqlite-execute db "UPDATE conversation_summaries SET title = ? WHERE conversation_id = ?"
                              (list title id))
              (ignore-errors
                (sqlite-execute db "UPDATE conversations_fts SET title = ? WHERE conversation_id = ?"
                                (list title id))))
          (sqlite-close db))))
    (setq lispm-antigravity--cache nil)))

(defun lispm-antigravity--session-paths (id)
  "Return the files and directories recording conversation ID."
  (let ((paths (list (lispm-antigravity--annotation-file id)
                     (expand-file-name (format "chapter_titles/%s.pb" id)
                                       lispm-antigravity-state-directory)
                     (expand-file-name (format "conversations/%s.db" id)
                                       lispm-antigravity-state-directory)
                     (expand-file-name (format "conversations/%s.db-wal" id)
                                       lispm-antigravity-state-directory)
                     (expand-file-name (format "conversations/%s.db-shm" id)
                                       lispm-antigravity-state-directory)
                     (expand-file-name (format "brain/%s" id)
                                       lispm-antigravity-state-directory)
                     (expand-file-name (format "worktrees/%s" id)
                                       lispm-antigravity-state-directory))))
    (seq-filter #'file-exists-p paths)))

(defun lispm-antigravity--delete (session)
  "Delete Antigravity SESSION from its summary store and remove its files."
  (let ((id (plist-get session :id))
        (state-file (lispm-antigravity--state-file)))
    (when (and (sqlite-available-p) (file-writable-p state-file))
      (let ((db (sqlite-open state-file)))
        (unwind-protect
            (progn
              (sqlite-execute db "DELETE FROM conversation_summaries WHERE conversation_id = ?"
                              (list id))
              (ignore-errors
                (sqlite-execute db "DELETE FROM conversations_fts WHERE conversation_id = ?"
                                (list id))))
          (sqlite-close db))))
    (dolist (path (lispm-antigravity--session-paths id))
      (if (file-directory-p path)
          (delete-directory path t t)
        (delete-file path t)))
    (setq lispm-antigravity--cache nil)))

(defun lispm-antigravity--transcript (session)
  "Return the transcript file of Antigravity SESSION."
  (let ((id (plist-get session :id)))
    (or (when-let* ((file (plist-get session :file))
                    ((file-readable-p file)))
          file)
        (lispm-antigravity--transcript-file id)
        (user-error "Antigravity has no transcript for session %s" id))))

;;;; Waiting for its user, and work details

(defun lispm-antigravity--terminal-transcript (terminal)
  "Return the transcript file of the session the Antigravity TERMINAL runs."
  (when-let* ((id (if (fboundp 'lispm-agents-terminal-session-id)
                      (lispm-agents-terminal-session-id terminal)
                    (buffer-local-value 'lispm-agents-buffer-session-id terminal))))
    (or (when-let* ((file (plist-get (lispm-agents-known-session 'antigravity id) :file))
                    ((file-readable-p file)))
          file)
        (lispm-antigravity--transcript-file id))))

(defun lispm-antigravity-turn-ended-p (objects)
  "Return non-nil if OBJECTS, the last records of an Antigravity transcript,
end a turn: the last step is a completed `PLANNER_RESPONSE' calling no tools."
  (let ((last (seq-find (lambda (object)
                          (member (plist-get object :type)
                                  '("USER_INPUT" "PLANNER_RESPONSE" "GENERIC")))
                        (reverse objects))))
    (and (equal (plist-get last :type) "PLANNER_RESPONSE")
         (equal (plist-get last :status) "DONE")
         (null (plist-get last :tool_calls)))))

(defun lispm-antigravity--db-idle-p (id)
  "Return non-nil when conversation ID is idle in Antigravity's summary store."
  (let ((state-file (lispm-antigravity--state-file)))
    (when (and (sqlite-available-p) (file-readable-p state-file))
      (let ((db (sqlite-open state-file)))
        (unwind-protect
            (pcase (car (sqlite-select
                         db
                         "SELECT status, not_fully_idle FROM conversation_summaries WHERE conversation_id = ?"
                         (list id)))
              (`("CASCADE_RUN_STATUS_IDLE" 0) t))
          (sqlite-close db))))))

(defun lispm-antigravity--ready-p (terminal)
  "Return non-nil when the Antigravity of TERMINAL waits for its user."
  (when-let* ((id (if (fboundp 'lispm-agents-terminal-session-id)
                      (lispm-agents-terminal-session-id terminal)
                    (buffer-local-value 'lispm-agents-buffer-session-id terminal))))
    (pcase (lispm-agents-known-turn 'antigravity id)
      ('ended t)
      ('working nil)
      (_ (if-let* ((file (lispm-antigravity--terminal-transcript terminal)))
             (lispm-antigravity-turn-ended-p (lispm-agents-read-json-tail file))
           (lispm-antigravity--db-idle-p id))))))

(defun lispm-antigravity--work (objects)
  "Return work recorded in the latest turn of Antigravity tail OBJECTS."
  (let (turn turn-time seen calls pending)
    (dolist (object objects)
      (let ((type (plist-get object :type))
            (step (plist-get object :step_index)))
        (cond
         ((or (equal type "USER_INPUT")
              (and (not seen) (equal type "SYSTEM_MESSAGE")))
          (setq turn (and step (format "%s" step))
                turn-time (lispm-antigravity--parse-time (plist-get object :created_at))
                seen t calls nil pending nil))
         ((and seen (equal type "PLANNER_RESPONSE"))
          (dolist (call pending)
            (when (eq (plist-get call :status) 'running)
              (setf (plist-get call :status) 'done)))
          (setq pending nil)
          (let ((index 0))
            (dolist (tool (plist-get object :tool_calls))
              (let ((record (list :id (format "%s:%d" (or step 0) index)
                                  :name (plist-get tool :name)
                                  :input-summary (lispm-agents-work-input-summary
                                                  (plist-get tool :args))
                                  :status 'running)))
                (push record calls)
                (setq pending (append pending (list record)))
                (setq index (1+ index))))))
         ((and seen (equal type "GENERIC") pending)
          (let ((call (pop pending)))
            (setf (plist-get call :status)
                  (if (equal (plist-get object :status) "ERROR") 'error 'done)))))))
    (list :turn turn :turn-time turn-time
          :turn-ended (and (lispm-antigravity-turn-ended-p objects) t)
          :pending-calls (seq-filter (lambda (call) (eq (plist-get call :status) 'running)) calls)
          :recent-calls (seq-take (seq-remove (lambda (call)
                                                (eq (plist-get call :status) 'running))
                                              calls)
                                  20)
          :files nil)))

(defun lispm-antigravity--work-details (terminal)
  "Return the work known from the bounded transcript tail of TERMINAL."
  (when-let* ((file (lispm-antigravity--terminal-transcript terminal)))
    (lispm-agents-cached-work file #'lispm-antigravity--work)))

;;;; Emacs's state, over MCP

(cl-defstruct (lispm-antigravity-mcp (:include lispm-agents-mcp)
                                     (:constructor lispm-antigravity-mcp--make))
  "The MCP server of an Antigravity terminal.")

(defun lispm-antigravity--token-file (server)
  "Return the file holding SERVER's token for the bridge."
  (expand-file-name (format "%d.token" (lispm-agents-mcp-port server))
                    lispm-antigravity-token-directory))

(defun lispm-antigravity--write-token-file (server)
  "Write SERVER's token to a file only its owner can read."
  (let ((file (lispm-antigravity--token-file server)))
    (with-file-modes #o700
      (make-directory lispm-antigravity-token-directory t))
    (ignore-errors (delete-file file))
    (with-file-modes #o600
      (write-region (lispm-agents-mcp-token server) nil file nil 'silent))))

(cl-defmethod lispm-agents-context-environment ((server lispm-antigravity-mcp))
  "Point the bridge Antigravity runs at SERVER and its token file."
  (list (format "LISPM_ANTIGRAVITY_MCP_URL=http://127.0.0.1:%d/mcp"
                (lispm-agents-mcp-port server))
        (format "LISPM_ANTIGRAVITY_MCP_TOKEN_FILE=%s"
                (lispm-antigravity--token-file server))))

(cl-defmethod lispm-agents-context-stop ((server lispm-antigravity-mcp))
  "Stop SERVER and remove its token file."
  (cl-call-next-method)
  (ignore-errors (delete-file (lispm-antigravity--token-file server))))

(defun lispm-antigravity-configured ()
  "Return how Antigravity's `mcp_config.json' names the MCP server `emacs':
`this' when it runs this package's bridge, `other' when it runs something
else, and nil when there is none or it is disabled."
  (when (file-readable-p lispm-antigravity-config-file)
    (with-temp-buffer
      (insert-file-contents lispm-antigravity-config-file)
      (when-let* ((json (ignore-errors
                          (json-parse-buffer :object-type 'plist
                                             :array-type 'list
                                             :null-object nil
                                             :false-object nil)))
                  (servers (plist-get json :mcpServers))
                  (emacs (plist-get servers :emacs))
                  ((not (eq (plist-get emacs :disabled) t)))
                  (command (or (plist-get emacs :command)
                               (plist-get emacs :url))))
        (if (equal command lispm-antigravity-bridge) 'this 'other)))))

(defun lispm-antigravity--context (directory)
  "Serve Emacs's state to an Antigravity terminal running in DIRECTORY."
  (let ((server (lispm-agents-mcp-start "antigravity-mcp"
                                        #'lispm-antigravity-mcp--make directory)))
    (lispm-antigravity--write-token-file server)
    (pcase (lispm-antigravity-configured)
      ('this)
      ('other (message "Antigravity runs another server as its MCP server `emacs': %s"
                       "run M-x lispm-antigravity-setup to point it at this package's bridge"))
      (_ (message "Antigravity cannot see Emacs's state until M-x lispm-antigravity-setup runs once")))
    server))

(defun lispm-antigravity-setup-arguments ()
  "Return the arguments registering the bridge as Antigravity's server `emacs'."
  (list "mcp" "add" "emacs" lispm-antigravity-bridge))

(defun lispm-antigravity-setup ()
  "Have Antigravity register this package's bridge as its MCP server `emacs'."
  (interactive)
  (let ((program (or (executable-find lispm-antigravity-program)
                     (user-error "Antigravity is not installed: `%s' is not on PATH"
                                 lispm-antigravity-program))))
    (with-temp-buffer
      (let* ((process-environment (lispm-agents-independent-environment process-environment))
             (status (apply #'call-process program nil t nil
                            (lispm-antigravity-setup-arguments))))
        (unless (eq status 0)
          (user-error "agy %s: %s"
                      (string-join (lispm-antigravity-setup-arguments) " ")
                      (string-trim (buffer-string))))
        (message "%s" (string-trim (buffer-string)))))))

;;;; The agent

(defun lispm-antigravity-new (&optional directory)
  "Start a new Antigravity session in DIRECTORY."
  (interactive (list (lispm-agents-read-directory 'antigravity)))
  (lispm-agents-new 'antigravity directory))

(lispm-agents-define
 'antigravity
 :name "Antigravity"
 :icon `(lispm-agents-image-icon
         ,(expand-file-name "icons/antigravity.png" lispm-antigravity-directory) nil)
 :text-icon "✦"
 :command #'lispm-antigravity-new
 :key "y"
 :program lispm-antigravity-program
 :sessions #'lispm-antigravity-sessions
 :rename #'lispm-antigravity--rename
 :delete #'lispm-antigravity--delete
 :transcript #'lispm-antigravity--transcript
 :resumable #'lispm-antigravity--resumable-p
 :worktree #'lispm-antigravity--worktree
 :resume-arguments (lambda (id) (list "--conversation" id))
 :model-arguments (lambda (model) (list "--model" model))
 :context #'lispm-antigravity--context
 :ready #'lispm-antigravity--ready-p
 :work-details #'lispm-antigravity--work-details)

(defun lispm-antigravity-unload-function ()
  "Take Antigravity away from the editor's agents, as this package is unloaded."
  (lispm-agents-undefine 'antigravity)
  ;; Unloading goes on as usual.
  nil)

(provide 'lispm-antigravity)
;;; lispm-antigravity.el ends here
