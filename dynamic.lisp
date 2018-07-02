;; Executa uma ação dinâmica
;; property: Propriedade dinâmica
;; value: Valor da ação dinâmica
;; Return: nil
(defun action (property value / blocks document SetActionBlock)

  (defun SetActionBlock (name blocks / block)
    (vlax-for block (vla-Item blocks name)
      (if (= (vla-get-ObjectName block) "AcDbBlockReference")
    (progn
      (if (= :vlax-true (vla-get-isdynamicblock block))
        (SetDynamicProperty block property value)
      )
      (SetActionBlock (vla-get-name block) blocks)
    )
      )
    )
  )

  (setq document (vla-get-activeDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-Blocks document))
  (SetActionBlock "crane" blocks)
  (vla-regen document acAllViewports)
  (princ)
)

;; Percorre recursivamente os blocos em busca de blocos dinâmicos e set a ação
;; name: Nome do bloco
;; blocks: Coleção de blocos
;; Return: nil


;; Seta o valor de uma ação dinâmica
;; block: Bloco dinâmico
;; property: Propriedade dinâmica
;; value: Valor da propriedade
(defun SetDynamicProperty (block property value)
  (setq property (strcase property))
  (vl-some
    '(lambda (x)
       (if (= property (strcase (vla-get-propertyname x)))
     (progn
       (vla-put-value
         x
         (vlax-make-variant
           value
           (vlax-variant-type (vla-get-value x))
         )
       )
       (cond (value)
         (t)
       )
     )
       )
     )
    (vlax-invoke block 'getdynamicblockproperties)
  )
)

;; Reseta todos os blocos dinâmicos
(defun reset (/ blocks document ResetBlock)
  (defun ResetBlock (name blocks)
    (vlax-for block (vla-Item blocks name)
      (if (= (vla-get-ObjectName block) "AcDbBlockReference")
    (progn
      (if (= :vlax-true (vla-get-isdynamicblock block))
        (vla-ResetBlock block)
      )
      (ResetBlock (vla-get-name block) blocks)
    )
      )
    )
  )

  (setq document (vla-get-activeDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-Blocks document))
  (ResetBlock "crane" blocks)
  (vla-purgeall document)
  (vla-regen document acAllViewports)
  (princ)
)