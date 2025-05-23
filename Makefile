# lucidplan - the CI/CD orchestrator written in Guile Scheme

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# lucidplan is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# lucidplan is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.

# Code:

.PHONY: test dir-install dir-uninstall dev server production-server fmt init-db compile-i18n new-i18n join-i18n init-i18n-nl init-i18n-en merge-i18n-nl merge-i18n-en

# Constants

DEPLOY_WWW=/srv/http/jointhefreeworld.org

LUCIDPLAN_LOCATION="/etc/lucidplan"
LUCIDPLAN_DB="lucidplan.db"
COPYRIGHT_HOLDERS="Josep Bigorra"
I18N_LOCATION="sys/i18n/locale"

# System details
GUIX_VERSION := $(shell guix --version)

# Logging helpers

define lucidplan-log-info
	@printf "\n\033[1m[INFO] $(shell date --rfc-3339=seconds) - %s\033[0m\n" $(1)
endef

# i18n helpers

# Define a function for compiling the translation file (.po) into a machine-readable (.mo) format
# Arguments: $(1) represents the language code (e.g., "en_US", "nl_NL", etc.)
# Use msgfmt to compile the .po file into a .mo file for the specified language
# The output .mo file will be placed in the LC_MESSAGES directory under the language's folder
define lucidplan-msgfmt
	msgfmt -v -o $(I18N_LOCATION)/$(1)/LC_MESSAGES/translate.mo \
		$(I18N_LOCATION)/$(1)/LC_MESSAGES/translate.po
endef

# Define a function to initialize a new translation file from a template (.pot) for a specific language
# Arguments: $(1) represents the language code (e.g., "en_US", "nl_NL", etc.)
# Use msginit to generate a .po file from the .pot template for the specified language
# The -l option sets the language, and --no-translator means no translator is pre-filled in the .po file
# The generated .po file is placed in the LC_MESSAGES directory for the given language
define lucidplan-msginit
	mkdir -p $(I18N_LOCATION)/$(1)/LC_MESSAGES
	msginit -l $(1).UTF8 -o $(I18N_LOCATION)/$(1)/LC_MESSAGES/translate.po \
		-i $(I18N_LOCATION)/translate.pot --no-translator
endef

# Define a function to merge translation updates from the template (.pot) into an existing .po file
# Arguments: $(1) represents the language code (e.g., "en_US", "nl_NL", etc.)
# Use msgmerge to update the specified language's .po file with new translations from the template (.pot)
# The -U flag updates the .po file in place
# This ensures the .po file stays up-to-date with the latest changes in the template
define lucidplan-merge
	msgmerge -U $(I18N_LOCATION)/$(1)/LC_MESSAGES/translate.po \
		$(I18N_LOCATION)/translate.pot
endef

# Define a function to extract translatable strings from Scheme source files using xgettext
# Arguments: $(1) should be either "--join-existing" for updating template
#   or "" to start from scratch.
# Run xgettext to extract strings marked for translation from Scheme source files
define lucidplan-xgettext
	xgettext --default-domain=translate \
		--output=$(I18N_LOCATION)/translate.pot \
		--language=Scheme \
		--keyword=G_ \
		--sort-by-file \
		$(1) \
		--copyright-holder=$(COPYRIGHT_HOLDERS) \
		--package-name="lucidplan" \
		lib/lucidplan/job/*.scm \
		lib/lucidplan/*.scm \
		app/controllers/*.scm \
		app/views/*.scm \
		app/api/*.scm \
		ENTRY \
		--from-code UTF-8
endef


# Target to prepare directories for the lucidplan project
# Create the necessary directory for job cloning, if it doesn't exist
# Change ownership of the lucidplan location to the current user
dir-install:
	$(call lucidplan-log-info,"preparing directories for lucidplan")
	sudo mkdir -pv $(LUCIDPLAN_LOCATION)/job-clone || true
	sudo chown -R $(shell whoami) $(LUCIDPLAN_LOCATION)

# Target to clean up the lucidplan installation (remove job files)
# Remove all files in the lucidplan location, ignoring errors
dir-uninstall:
	$(call lucidplan-log-info,"cleaning lucidplan installation")
	rm -rfv $(LUCIDPLAN_LOCATION)/* || true

watchexec:
	watchexec -i "dist/**" -r -e scm,css,js -- "make server"


# Target to format source code files according to the style guide
# Find all .scm files and format them using 'guix style'
fmt:
	$(call lucidplan-log-info,"formatting all Scheme files")
	find . -maxdepth 8 -name "*.scm" -type f -exec guix style -f {} \;

# Target to start a development server for lucidplan
# Use watchexec to watch for file changes and restart the server on changes
dev:
	@-make fmt
	$(call lucidplan-log-info,"starting development server for lucidplan")
ifdef GUIX_VERSION
	guix shell -m manifest.scm -- make watchexec
else
	make watchexec
endif


# Target to start the lucidplan server
# Start the server with the default configuration
server:
	$(call lucidplan-log-info,"starting lucidplan server")
	guix shell -m manifest.scm -- art work

# Target to start the lucidplan server with production configuration
production-server:
	$(call lucidplan-log-info,"starting production lucidplan server")
	guix shell -m manifest.scm -- art work -c conf/production.conf


# Initialize a new database and enable WAL mode for higher concurrent writes to SQLite 3
init-db:
	$(call lucidplan-log-info,"initializing a new database")
	-rm -v $(LUCIDPLAN_DB) $(LUCIDPLAN_DB)-shm $(LUCIDPLAN_DB)-wal
	guix shell -m manifest.scm -- sqlite3 lucidplan.db 'PRAGMA journal_mode=WAL;'

# Compile translation files to .mo files
compile-i18n:
	$(call lucidplan-log-info,"compiling translation files")
	$(call lucidplan-msgfmt,"en_US")
	$(call lucidplan-msgfmt,"nl_NL")
	$(call lucidplan-msgfmt,"es_ES")

# Create new i18n template from source code
new-i18n:
	$(call lucidplan-log-info,"creating new i18n template from source code")
	$(call lucidplan-xgettext,"")

# Update i18n template from source code
join-i18n:
	$(call lucidplan-log-info,"updating i18n template from source code")
	$(call lucidplan-xgettext,"--join-existing")

# en_US - English language targets
init-i18n-en:
	$(call lucidplan-msginit,"en_US")
merge-i18n-en:
	$(call lucidplan-merge,"en_US")

# nl_NL - Dutch language targets
init-i18n-nl:
	$(call lucidplan-msginit,"nl_NL")
merge-i18n-nl:
	$(call lucidplan-merge,"nl_NL")

# es_ES - Spanish language targets
init-i18n-es:
	$(call lucidplan-msginit,"es_ES")
merge-i18n-es:
	$(call lucidplan-merge,"es_ES")

# Development REPL
repl:
	guix shell -m manifest.scm -- guile  \
		-L lib -L . \
		-c '((@ (ares server) run-nrepl-server))'

# deploy: Deploys the manual HTML to a web server directory.
deploy:
	guix shell -m manifest.scm -- emacs -Q --batch \
		--eval '(progn (find-file "$(shell realpath ./docs/manual/en-GB.org)")(org-html-export-to-html)(message "finished publishing LucidPlan manual"))'
	mkdir -pv $(DEPLOY_WWW)/manuals/lucidplan
	rm -rfv $(DEPLOY_WWW)/manuals/lucidplan/index.html
	mv -v docs/manual/en-GB.html $(DEPLOY_WWW)/manuals/lucidplan/index.html
