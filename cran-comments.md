## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
- Local note is from time verification error
- Apologies for quick successive releases, but a bug in the client secret validation slipped through last version. It incorrectly errored if the client secret contained special characters, and the system does occasionally generate such secrets. The validation now only checks for length to avoid this issue.
