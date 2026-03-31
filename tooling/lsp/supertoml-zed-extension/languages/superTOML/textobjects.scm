(comment)+ @comment.around
(table "[" (_) "]"
    (_)* @class.inside) @class.around

(table_array_element "[[" (_) "]]"
    (_)* @class.inside) @class.around
