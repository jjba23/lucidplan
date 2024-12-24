# LucidPlan

# Copyright (C) 2024 Josep Jesus Bigorra Algaba

# lucidplan is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# lucidplan is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with lucidplan. If not, see <https://www.gnu.org/licenses/>.

.PHONY: server fmt

server:
	art work
fmt:
	find . -maxdepth 5 -name "*.scm" -type f -exec guix style -f {} \;
dev:
	watchexec -i "dist/**" -r -e scm,css,js -- "make server"
