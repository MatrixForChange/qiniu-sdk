# qiniu-sdk
A SDK for qiniu.com (Racket)
# Installation
```
raco pkg install qiniu-sdk
```
# Simple Usage
```
(require qiniu-sdk)
(current-qiniu-access-token #"your access token")
(current-qiniu-secret-token #"your secret token")
(current-qiniu-expired-time 30)   ;30 days
(qiniu-upload "your buket" (open-input-file "test.jpg"))
(qiniu-upload "your buket" (open-input-file "test.jps") #:key "your resource key")
```