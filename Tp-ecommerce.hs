-- take :: Int -> [a] -> [a]
-- drop :: Int -> [a] -> [a]
-- head :: [a] -> a 
-- elem :: a -> [a] -> Bool
-- reverse :: [a] -> [a]

aplicarDescuento :: Fractional a => (String,a)->a->(String,a)
aplicarDescuento (nombre,precio) descuento =  
    (nombre,precio-(precio*(descuento/100)))


aplicarCostoDeEnvio :: Fractional a => (String,a)->a->a
aplicarCostoDeEnvio (_,precio) costoenvio =  
    precio + costoenvio

precioTotal :: Fractional a => (String,a)->a->a->a->a
precioTotal (nombre,preciounitario) cantidad descuento costoenvio =
    aplicarCostoDeEnvio (nombre,(cantidad*).snd $ (aplicarDescuento (nombre,preciounitario) descuento)) costoenvio

productoDeLujo :: Fractional a => (String,a) -> Bool
productoDeLujo (nombre,_) =  
    (elem 'x' nombre) || (elem 'z' nombre) 

productoCodiciado :: Fractional a => (String,a)  -> Bool
productoCodiciado (nombre,_) =  
    length(nombre) > 10

productoCorriente :: Fractional a => (String,a) -> Bool
productoCorriente (nombre,_) =
    elem (head nombre) "aeiouAEIOU" 

productoDeElite :: Fractional a => (String,a) -> Bool
productoDeElite (nombre,precio) =
    (productoDeLujo (nombre,precio)) && (productoCodiciado (nombre,precio)) && (not(productoCorriente (nombre,precio)))

entregaSencilla :: String -> Bool 
entregaSencilla dia =  
    even.length $ dia

descodiciarProducto :: Fractional a => (String,a)->(String,a)
descodiciarProducto (nombre,precio) =
    (take 10 nombre,precio)

versionBarata :: Fractional a => (String,a) -> (String,a)
versionBarata (nombre,precio) =
    (reverse.fst.descodiciarProducto $ (nombre,0),precio)

productoXL :: Fractional a => (String,a) -> (String,a)
productoXL (nombre,precio) =
    (nombre ++ "XL",precio)

