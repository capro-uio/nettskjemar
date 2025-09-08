## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
- Local note is from time verification error
- Apologies for quick successive releases, but a bug in the client secret validation slipped through testing. It incorrectly errored if the client secret contained special characters, and the system does sporadically generate such secrets. The validation now only check for length, and the bug is fixed in 1.0.4.
