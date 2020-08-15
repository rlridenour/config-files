function html2org
         pandoc -o {$argv}.org {$argv}.html
end
