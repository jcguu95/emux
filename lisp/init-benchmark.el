;;; -*- lexical-binding: t; -*-

(use-package benchmark-init
  ;; 啟動完成後，執行 =M-x benchmark-init/show-durations= 或 =M-x
  ;; benchmark-init/show-durations-tree= 以查看詳細的啟動時間報告。報告會顯示
  ;; 每個載入項目的耗時。NOTE 重要: =benchmark-init= 必須在您的設定檔中盡可能
  ;; 早地載入，才能測量所有 =load= 和 =require= 調用。
  :config
  ;; 啟動後停用資料收集
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

(provide 'init-benchmark)
