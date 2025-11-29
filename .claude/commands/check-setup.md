---
description: Check repository setup per global CLAUDE.md instructions
---

Perform the mandatory first actions from global CLAUDE.md:

1. Check if .gitignore exists and verify it ignores itself as the first entry
2. Verify .gitignore is NOT in the remote repository (git ls-tree check)
3. If .gitignore is in remote or doesn't ignore itself: Fix it immediately
4. Check if CLAUDE.md exists and verify it's NOT in the remote repository
5. If CLAUDE.md exists, verify it's in .gitignore
6. If CLAUDE.md is in remote or not in .gitignore: Fix it immediately

Report any issues found and fix them automatically.
