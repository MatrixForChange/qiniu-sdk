# qiniu-sdk
A SDK for qiniu.com (Racket)
# Simple Usage
```
(current-qiniu-access-token #"your access token")
(current-qiniu-secret-token #"your secret token")
(current-qiniu-expired-time 30)   ;30 days
(qiniu-upload "your buket" (open-input-file "test.jpg"))
```