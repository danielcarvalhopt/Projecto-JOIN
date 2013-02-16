module JOIN where 

import Char
import IO
import Time
import System.IO
import System
import System.Process
import Control.Monad
import System.Directory (getDirectoryContents)
import System.Console.ANSI

type Nome = String
type Email = String
type Curso = String
type Numaluno = Integer
type Universidade = String
type Cursouni = String
type Empresa = String
type Jantarconf = Integer
type Almoco = Integer
type Inscr = [String]
type Inscroutro = String
type Date = ((Integer,Integer,Integer),(Integer,Integer,Integer,Integer))
type Inscricao = (Nome, Email, Curso, Numaluno, Universidade, Cursouni, Empresa, Jantarconf, Almoco, Inscr, Inscroutro, Date)
type Inscricoes = [Inscricao]
------------------------------------------------------------------------
-- LEITURA FICHEIRO
-- LEITURA FICHEIRO
-- LEITURA FICHEIRO
-- LEITURA FICHEIRO
------------------------------------------------------------------------

-- Lê ficheiro "ficheiro" e dá como output uma lista com as linhas 
-- do ficheiro em forma de strings. 	 
leituraficheiro :: FilePath -> IO [String]
leituraficheiro ficheiro = do
         fileContent <- readFile ficheiro;
		 let dados = lines fileContent;
         return dados;
         
------------------------------------------------------------------------         
-- INÍCIO ESTRUTURAÇÃO E CORRECÇÃO DE DADOS
-- INÍCIO ESTRUTURAÇÃO E CORRECÇÃO DE DADOS
-- INÍCIO ESTRUTURAÇÃO E CORRECÇÃO DE DADOS
-- INÍCIO ESTRUTURAÇÃO E CORRECÇÃO DE DADOS
------------------------------------------------------------------------

----------------------------
-- Estruturação dos Dados --
----------------------------

-- Percorre uma string até encontrar o caractér ',' e separa a string 
-- nesse local, formando um túplo com duas strings, uma até ao local 
-- do caractér ',' e outra com o resto da string.
-- Para além disso ignora o caractér ',' se este se encontrar dentro 
-- de uma já String.

stringtostrings lista = tratastring lista False

tratastring [] _ = ([],[])
tratastring (h:t) state | h == '"' = tratastring t (not state)
					 | state == True = let (a,b) = tratastring t state in (h:a,b)
		             | h == ',' && state == False = ([], t)
        	         | otherwise = let (a,b) = tratastring t state in (h:a,b)
        	    
               	    
-- Aplica a função stringtostrings a uma String, partindo uma String em 
-- todos os locais onde exista o caractér ',' recursivamente.

listastrings :: String -> [String]
listastrings [] = []
listastrings lista = (a: listastrings b)
				where (a,b) = stringtostrings lista
		   
		   
-- Aplica a função listastrings a uma lista de strings transformado 
-- essa lista numa lista de listas de strings.

listasdestrings :: [String] -> [[String]]
listasdestrings [] = []
listasdestrings (h:t) = ((listastrings h):listasdestrings t)	


-- Converte uma lista num tuplo

listaparatuplo :: [a] -> (a,a,a,a,a,a,a,a,a,a,a,a)
listaparatuplo [a,b,c,d,e,f,g,h,i,j,k,l] = (a,b,c,d,e,f,g,h,i,j,k,l)


-- Converte uma lista de listas numa lista de tuplos

listalistasparatuplo :: [[a]] -> [(a,a,a,a,a,a,a,a,a,a,a,a)]
listalistasparatuplo [] = []
listalistasparatuplo (h:t) = (listaparatuplo h: listalistasparatuplo t)


-- Aplica a função listalistasparatuplo à lista de listas de strings 
-- obtida na função listasdestrings

ficheiroparalistatuplos lista = listalistasparatuplo (listasdestrings lista)

-- Elimina o primeiro tuplo que contém as variáveis das inscrições.
eliminavariaveis (h:t) = t

eliminavariaveislista lista = eliminavariaveis (ficheiroparalistatuplos lista)


-- Aplica a função ficheiroparalistatuplos ao resultado do tipo 
-- IO [String] da função linhasfich.

ioStringparaString ficheiro = do
				lista <- leituraficheiro ficheiro
				return $ eliminavariaveislista lista


-------------------------
-- Correcção dos Dados --
-------------------------

		--------------------------
		-- Correcção Inscrições --
		--------------------------
-- FALTA CORRIGIR CARACTERES \xc3,\xc2\xba
-- Corrige campo dos motivos da inscrição apagando os caractéres '[',
-- ']', '''. Para além disso elimina o caractér ' ' que está fora das 
-- plicas.

corrigestringinscricoes lista = corrigeplicasinscricao (aux lista)

aux lista = auxb lista False	
			  
auxb [] _ = []
auxb (h:t) state | h == '\'' = (h:auxb t (not state))
				| h == ' ' && state == False = auxb t state
				| h == '[' || h == ']' && state == False = auxb t state
				| otherwise = (h:auxb t state)
				
corrigeplicasinscricao [] = []
corrigeplicasinscricao  (h:t) | h == '\'' = corrigeplicasinscricao t
							  | otherwise = (h: corrigeplicasinscricao t)

	  	
-- Aplica a função corrigestringinscricoes à lista de tuplos.
corrigestringinscricoeslista [] = []
corrigestringinscricoeslista ((a,b,c,d,e,f,g,h,i,ins,k,l):t) = ((a,b,c,d,e,f,g,h,i,(corrigestringinscricoes ins),k,l): corrigestringinscricoeslista t)
			  
			  
--Divide a String em várias strings onde encontra o caractér ','
tratastringinscricoes [] = ([],[])
tratastringinscricoes lista@(h:t) | h == ',' = ([], t)
				  | otherwise = let (a,b) = tratastringinscricoes t in (h:a,b)        	         				  


-- Torna as razões da inscrição que estão numa string em várias strings
stringinscricoesparalista :: String -> [String]
stringinscricoesparalista [] = []
stringinscricoesparalista lista = (a: stringinscricoesparalista b)
				where (a,b) = tratastringinscricoes lista


-- Aplica a função stringinscricoesparalista ao campo dos motivos das incrições
inscricoesparalista [] = []				  
inscricoesparalista ((a,b,c,d,e,f,g,h,i,ins,k,l):t) = ((a,b,c,d,e,f,g,h,i,(stringinscricoesparalista ins),k,l): inscricoesparalista t)


-- Aplica  a função inscricoesparalista à função corrigeinslista
corrigeinscricoes lista = inscricoesparalista (corrigestringinscricoeslista lista)

fimcorreccaoinscricoes ficheiro = do
				lista <- ioStringparaString ficheiro
				return $ corrigeinscricoes lista
				
-- FALTA CORRIGIR CARACTERES \xc3,\xc2\xba

		-------------------------------
		-- Correcção Número de Aluno --
		-------------------------------
		
-- Lê o campo Número de aluno
leituranumero :: String -> String
leituranumero [] = "0"
leituranumero x = x

-- Corrige campo Número de aluno
corrigestringnumeroaluno :: String -> String
corrigestringnumeroaluno [] = []
corrigestringnumeroaluno (h:t) | (ord h >= 48 && ord h <= 57) = (h: corrigestringnumeroaluno t)
			| otherwise = corrigestringnumeroaluno t
-- Aplica as funções corrigestringnumeroaluno e leituranumero à lista de tuplos
corrigenumeroaluno [] = []				  
corrigenumeroaluno ((a,b,c,num,e,f,g,h,i,j,k,l):t) = ((a,b,c,corrigestringnumeroaluno (leituranumero num),e,f,g,h,i,j,k,l): corrigenumeroaluno t)


fimcorreccaonumeroaluno ficheiro = do
				lista <- fimcorreccaoinscricoes ficheiro
				return $ corrigenumeroaluno lista

		--------------------
		-- Correcção Data --
		--------------------
		
-- Divide a String em várias strings onde encontra o caractér ','
tratastringdatahora [] = ([],[])
tratastringdatahora lista@(h:t) | h == ' ' = ([], t)
				  | otherwise = let (a,b) = tratastringdatahora t in (h:a,b)
				  
-- Torna a data e hora que estão numa string em várias strings
listastringsdatahora :: String -> [String]
listastringsdatahora [] = []
listastringsdatahora lista = (a: listastringsdatahora b)
				where (a,b) = tratastringdatahora lista		
				
-- Torna a data num tuplo 
datafix [] = ([],[])
datafix lista@(h:t) | h == '/' = ([], t)
				  | otherwise = let (a,b) = datafix t in (h:a,b)				  
datafixlista :: String -> [String]				  
datafixlista [] = []
datafixlista lista = (a: datafixlista b)
				where (a,b) = datafix lista
datafixlistaparatuplo :: [String] -> (Integer,Integer,Integer)	
datafixlistaparatuplo (ano:mes:dia:[]) = (read ano,read mes,read dia)	

datafixtuplo lista = datafixlistaparatuplo (datafixlista lista)				
				
-- Torna a hora num tuplo 
horafix [] = ([],[])
horafix lista@(h:t) | h == ':' || h== '.' = ([], t)
				    | otherwise = let (a,b) = horafix t in (h:a,b)
horafixlista :: String -> [String]				  
horafixlista [] = []
horafixlista lista = (a: horafixlista b)
				where (a,b) = horafix lista	

horafixlistaparatuplo :: [String] -> (Integer,Integer,Integer,Integer)				
horafixlistaparatuplo (hora:min:seg:miliseg:[]) = ( ((read hora)-1),read min,read seg,read miliseg)	

horafixtuplo lista = horafixlistaparatuplo (horafixlista lista)					

-- Aplica as funções datafixtuplo e horafixtuplo ao campo da data e hora				
datahoraparatuplo (date:hora:gmt:[]) = ((datafixtuplo date),(horafixtuplo hora))				
		
-- Aplica a função datahoraparatuplo à lista de strings com a data, hora e timezone			
listatuplosdatahora lista = datahoraparatuplo (listastringsdatahora lista)						

-- Aplica a função listatuplosdatahora a toda a lista com as inscrições		
corrigedatahora [] = []				  
corrigedatahora ((a,b,c,d,e,f,g,h,i,j,k,datahora):t) = ((a,b,c,d,e,f,g,h,i,j,k,(listatuplosdatahora datahora)): corrigedatahora t)

fimcorreccaodatahora ficheiro = do
				lista <- fimcorreccaonumeroaluno ficheiro
				return $ corrigedatahora lista
		
		
		-------------
		-- Tipagem --
		-------------
		
tipagem :: [(String,String,String,String,String,String,String,String,String,[String],String,Date)] -> [Inscricao]
tipagem [] = []
tipagem ((nome,email,curso,numaluno,universidade,cursouni,empresa,jantarconf,almoco,inscr,inscroutro, date):t) = ((nome,email,curso, read numaluno,universidade,cursouni,empresa,read jantarconf, read almoco,inscr,inscroutro, date): tipagem t)		
		
fimtipagem ficheiro = do
					lista <- fimcorreccaodatahora ficheiro
					return $ tipagem lista


------------------------------------------------------------------------         
-- INÍCIO VALIDAÇÃO DE DADOS
-- INÍCIO VALIDAÇÃO DE DADOS
-- INÍCIO VALIDAÇÃO DE DADOS
-- INÍCIO VALIDAÇÃO DE DADOS
------------------------------------------------------------------------	
		
		----------------------------
		-- Comparação Data e Hora --
		----------------------------
{-		
comparacaodatahora :: Date -> Date -> Date 
comparacaodatahora a@((ano,mes,dia),(hora,min,seg,miliseg),tz) b@((anob,mesb,diab),(horab,minb,segb,milisegb),tzb) 
	| ano > anob = a
	| ano == anob && mes > mesb = a
	| ano == anob && mes == mesb && dia > diab = a
	| ano == anob && mes == mesb && dia == diab && hora > horab = a
	| ano == anob && mes == mesb && dia == diab && hora == horab && min > minb = a		
	| ano == anob && mes == mesb && dia == diab && hora == horab && min == minb && seg > segb = a
	| ano == anob && mes == mesb && dia == diab && hora == horab && min == minb && seg == segb && miliseg > milisegb = a
	|otherwise = b	
		
comparacaodatahorainscricao :: Inscricao -> Inscricao -> Inscricao
comparacaodatahorainscricao l1@(a,b,c,d,e,f,g,h,i,j,k,date) l2@(m,n,o,p,q,r,s,t,u,v,x,dateb) 
			| (comparacaodatahora date dateb == date) = l1
			| otherwise = l2
-}		
		
		--------------------------
		-- Candidaturas Válidas --
		--------------------------		

-- Verifica as candidaturas válidas de alunos
validaalunos :: Inscricoes -> Inscricoes
validaalunos [] = []	
validaalunos lista@(inscr@(nome,email,curso,num,uni,cursoext,g,h,i,j,k,l):t) = if  nome /= [] && email /= [] && num /= 0 && curso /= [] && cursoext == [] then (inscr:validaalunos t) else validaalunos t

fimvalidaalunos ficheiro =do
					x <- fimtipagem ficheiro
					return $ validaalunos x

-- Verifica as candidaturas válidas de Universitários
validauniversitarios :: Inscricoes -> Inscricoes
validauniversitarios [] = []
validauniversitarios lista@(inscr@(nome,email,curso,num,uni,cursoext,g,h,i,j,k,l):t) = if nome /= [] && email /= [] && uni /= [] && cursoext /= [] && curso == [] then inscr:validauniversitarios t else validauniversitarios t

fimvalidauniversitarios ficheiro = do
							x <- fimtipagem ficheiro
							return $ validauniversitarios x
							
-- Verifica as candidaturas válidas de Empresas
validaempresas :: Inscricoes -> Inscricoes
validaempresas [] = []
validaempresas lista@(inscr@(nome,email,c,d,e,f,emp,h,i,j,k,l):t) = if nome /= [] && email /= [] && emp /= [] && c == [] && e == [] && f == [] then inscr : validaempresas t else validaempresas t

fimvalidaempresas ficheiro = do
							x <- fimtipagem ficheiro
							return $ validaempresas x
		
invalidas :: Inscricoes -> Inscricoes							
invalidas [] = []
invalidas lista@(h:t) | (elem h (validaalunos lista) == False) && (elem h (validaempresas lista) == False)  && (elem h (validauniversitarios lista) == False) = h : invalidas t
					  | otherwise = invalidas t

fimvalinvalidas ficheiro = do
							x <- fimtipagem ficheiro
							return $ invalidas x
							
------------------------------------------------------------------------         
-- FIM VALIDAÇÃO DE DADOS
-- FIM VALIDAÇÃO DE DADOS
-- FIM VALIDAÇÃO DE DADOS
-- FIM VALIDAÇÃO DE DADOS
------------------------------------------------------------------------

auxfixjantaralmoco a | a == 1 = "Participa"
				     | a == 0 = "N\227o Participa"

fixjantaralmoco [] = []
fixjantaralmoco ((a,b,c,d,e,f,g,h,i,j,k,l):t) = (a,b,c,d,e,f,g,auxfixjantaralmoco h,auxfixjantaralmoco i,j,k,l):fixjantaralmoco t

	
inscrfix [] = []
inscrfix (h:t) | h == "Procura 1\\xc2\\xba emprego" = "Procura 1\186 emprego":inscrfix t 
			   | h == "Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas" = "Interesse pelas \225reas tem\225ticas":inscrfix t
			   | h == "Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado" = "Procura projecto/disserta\231\227o de mestrado":inscrfix t

datafinalfix ((ano,mes,dia),(hora,min,seg,ms)) = (show dia)++"-"++(show mes)++"-"++(show ano)++" "++(show hora)++":"++(show min)++":"++(show seg)++"."++(show ms)

inscrfinalfix [] = []++"--"
inscrfinalfix (h:t) = h++" / "++ inscrfinalfix t

finalfix [] = []
finalfix ((a,b,c,d,e,f,g,h,i,j,k,l):t) = (a,b,c,(show d),e,f,g,auxfixjantaralmoco h,auxfixjantaralmoco i,inscrfinalfix (inscrfix j),k,datafinalfix l): finalfix t		

finalfixalunos ficheiro = do 
					x <- fimvalidaalunos ficheiro
					return $ finalfix x
					
finalfixuniversitarios ficheiro = do 
					x <- fimvalidauniversitarios ficheiro
					return $ finalfix x

finalfixempresas ficheiro = do 
					x <- fimvalidaempresas ficheiro
					return $ finalfix x

finalfixinvalidas ficheiro = do
						x <- fimvalinvalidas ficheiro 
						return $ finalfix x
						
totalfix ficheiro = do
				x <- fimvalidaalunos ficheiro
				y <- fimvalidauniversitarios ficheiro
				z <- fimvalidaempresas ficheiro
				return $ x ++ y ++ z
						
					
------------------------------------------------------------------------         
-- CRIAÇÃO DE FICHEIROS FINAIS
-- CRIAÇÃO DE FICHEIROS FINAIS
-- CRIAÇÃO DE FICHEIROS FINAIS
-- CRIAÇÃO DE FICHEIROS FINAIS
------------------------------------------------------------------------


	
writealuno [] = []
writealuno ((a,b,c,d,e,f,g,h,i,j,k,l):t) = ("Nome: "++a++"\n"++"Email: "++b++"\n"++"Curso: "++c++"\n"++"N\186 Mecanogr\225fico: "++d++"\n"++"Jantar da Conf: "++h++"\n"++"Almo\231o no R.P.: "++i++"\n"++"Motivos da Inscri\231\227o: "++j++k++"\n"++"Inscrito em: "++l++"\n\n") ++ writealuno t
	
fimwritealuno ficheiro = do
				x <- finalfixalunos ficheiro
				return $ writealuno x

writeuniversitario [] = []
writeuniversitario ((a,b,c,d,e,f,g,h,i,j,k,l):t) = ("Nome: "++a++"\n"++"Email: "++b++"\n"++"Universidade: "++e++"\n"++"Curso: "++f++"\n"++"Jantar da Conf: "++h++"\n"++"Almo\231o no R.P.: "++i++"\n"++"Motivos da Inscri\231\227o: "++j++k++"\n"++"Inscrito em: "++l++"\n\n") ++ writeuniversitario t
	
fimwriteuniversitario ficheiro = do
						x <- finalfixuniversitarios ficheiro 
						return $ writeuniversitario x
						
writeempresa [] = []
writeempresa ((a,b,c,d,e,f,g,h,i,j,k,l):t) = ("Nome: "++a++"\n"++"Email: "++b++"\n"++"Filia\231\227o/Empresa: "++g++"\n"++"Jantar da Conf: "++h++"\n"++"Almo\231o no R.P.: "++i++"\n"++"Motivos da Inscri\231\227o: "++j++k++"\n"++"Inscrito em: "++l++"\n\n") ++ writeempresa t

fimwriteempresa ficheiro = do
						x <- finalfixempresas ficheiro 
						return $ writeempresa x							

writeinvalidas [] = []
writeinvalidas ((a,b,c,d,e,f,g,h,i,j,k,l):t) = ("Nome: "++a++"\n"++"Email: "++b++"\n"++"Curso: "++c++"\n"++"N\186 Mecanogr\225fico: "++d++"\n"++"Universidade: "++e++"\n"++"Curso externo: "++f++"\n"++ "Filia\231\227o/Empresa: "++g++"\n"++"Jantar da Conf: "++h++"\n"++"Almo\231o no R.P.: "++i++"\n"++"Motivos da Inscri\231\227o: "++j++k++"\n"++"Inscrito em: "++l++"\n\n") ++ writeinvalidas t

fimwriteinvalidas ficheiro = do
					x <- finalfixinvalidas ficheiro 
					return $ writeinvalidas x

fimalunos ficheiro = do
			x <- fimwritealuno ficheiro 
			writeFile "./Resultados/Alunos - Inscri\xc3\xa7\xc3\xb5\x65s V\xc3\xa1lidas.txt" x
			
fimuniversitarios ficheiro = do
			x <- fimwriteuniversitario ficheiro 
			writeFile "./Resultados/Universit\xc3\xa1rios - Inscri\xc3\xa7\xc3\xb5\x65s V\xc3\xa1lidas.txt" x
			
fimempresas ficheiro = do
			x <- fimwriteempresa ficheiro 
			writeFile "./Resultados/Empresas - Inscri\xc3\xa7\xc3\xb5\x65s V\xc3\xa1lidas.txt" x
			
fiminvalidas ficheiro = do
			 x <- fimwriteinvalidas ficheiro 
			 writeFile "./Resultados/Inscri\xc3\xa7\xc3\xb5\x65s Inv\xc3\xa1lidas.txt" x 

fim ficheiro = do
			fimalunos ficheiro;
			fimuniversitarios ficheiro;
			fimempresas ficheiro;
			fiminvalidas ficheiro;

------------------------------------------------------------------------         
-- CRIAÇÃO DE CRACHÁS
-- CRIAÇÃO DE CRACHÁS
-- CRIAÇÃO DE CRACHÁS
-- CRIAÇÃO DE CRACHÁS
------------------------------------------------------------------------

------------------------------------------------------------------------         
-- ESTATISTÍSTICAS
-- ESTATISTÍSTICAS
-- ESTATISTÍSTICAS
-- ESTATISTÍSTICAS
------------------------------------------------------------------------

-- Conta número de inscritos (alunos, universitários, empresas)
numeroinscritos [] = 0
numeroinscritos (h:t) = 1 + numeroinscritos t

contagemnumeroinscritos ficheiro = do
					x <- totalfix ficheiro
					return $ numeroinscritos x
-- Conta alunos
numeroalunos [] = 0
numeroalunos (h:t) = 1 + numeroalunos t

contagemnumeroalunos ficheiro = do 
						x <- finalfixalunos ficheiro
						return $ numeroalunos x	
-- Conta universitários
numerouniversitarios [] = 0
numerouniversitarios (h:t) = 1 + numerouniversitarios t

contagemnumerouniversitarios ficheiro = do 
						x <- finalfixuniversitarios ficheiro
						return $ numerouniversitarios x	
-- Conta empresas
numeroempresas [] = 0
numeroempresas (h:t) = 1 + numeroempresas t

contagemnumeroempresas ficheiro = do 
						x <- finalfixempresas ficheiro
						return $ numeroempresas x					
-- Conta os inscritos de cada curso
-- LEI
leiinscritos [] = 0
leiinscritos ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if c == "LEI" then 1+leiinscritos t else leiinscritos t 

contagemleiinscritos ficheiro = do
					x <- finalfixalunos ficheiro
					return $ leiinscritos x
-- LCC
lccinscritos [] = 0
lccinscritos ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if c == "LCC" then 1+lccinscritos t else lccinscritos t 

contagemlccinscritos ficheiro = do
					x <- finalfixalunos ficheiro
					return $ lccinscritos x
-- MI
miinscritos [] = 0
miinscritos ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if c == "MI" then 1+miinscritos t else miinscritos t 

contagemmiinscritos ficheiro = do
					x <- finalfixalunos ficheiro
					return $ miinscritos x
-- MEI
meiinscritos [] = 0
meiinscritos ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if c == "MEI" then 1+meiinscritos t else meiinscritos t 

contagemmeiinscritos ficheiro = do
					x <- finalfixalunos ficheiro
					return $ meiinscritos x
-- MERSCOM
merscominscritos [] = 0
merscominscritos ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if c == "MERSCOM" then 1+merscominscritos t else merscominscritos t 

contagemmerscominscritos ficheiro = do
					x <- finalfixalunos ficheiro
					return $ merscominscritos x
-- MBIO
mbioinscritos [] = 0
mbioinscritos ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if c == "MBIO" then 1+mbioinscritos t else mbioinscritos t 

contagemmbioinscritos ficheiro = do
					x <- finalfixalunos ficheiro
					return $ mbioinscritos x

-- Conta as áreas de interesse
-- 1º Emprego
inteemprego [] = 0
inteemprego ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if elem "Procura 1\\xc2\\xba emprego" j then 1 + inteemprego t else inteemprego t 

contagemintemprego ficheiro = do
							x <- totalfix ficheiro
							return $ inteemprego x
-- Dissertação de Mestrado / Projecto
intprojmest [] = 0
intprojmest ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if elem "Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado" j then 1 + intprojmest t else intprojmest t 

contagemintprojmest ficheiro = do
							x <- totalfix ficheiro
							return $ intprojmest x

-- Áreas temáticas
inttematico [] = 0
inttematico ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if elem "Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas" j then 1 + inttematico t else inttematico t 

contageminttematico ficheiro = do
							x <- totalfix ficheiro
							return $ inttematico x

-- Conta os inscritos no jantar da conf.
jantarconf [] = 0
jantarconf ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if h == 1 then 1 + jantarconf t else jantarconf t 										 
											 
contagemjantarconf ficheiro = do
						x <- totalfix ficheiro
						return $ jantarconf x
				
-- Conta os inscritos no Almoço no RP
almocorp [] = 0
almocorp ((a,b,c,d,e,f,g,h,i,j,k,l):t) = if i == 1 then 1 + almocorp t else almocorp t 										 
											 
contagemalmocorp ficheiro = do
						x <- totalfix ficheiro
						return $ almocorp x

------------------------------------------------------------------------         
-- CRIAÇÃO LATEX COM ESTATISTÍSTICAS
-- CRIAÇÃO LATEX COM ESTATISTÍSTICAS
-- CRIAÇÃO LATEX COM ESTATISTÍSTICAS
-- CRIAÇÃO LATEX COM ESTATISTÍSTICAS
------------------------------------------------------------------------
-- Cria um tuplo com os numeros estatisticos obtidos acima
stats file = do
		a <- contagemnumeroinscritos file
		b <- contagemnumeroalunos file
		c <- contagemnumerouniversitarios file
		d <- contagemnumeroempresas file
		e <- contagemleiinscritos file
		f <- contagemlccinscritos file
		g <- contagemmiinscritos file
		h <- contagemmiinscritos file
		i <- contagemmerscominscritos file
		j <- contagemmbioinscritos file
		k <- contagemintemprego file
		l <- contagemintprojmest file
		m <- contageminttematico file
		n <- contagemjantarconf file
		o <- contagemalmocorp file
		return $ (show a, show b, show c, show d, show e, show f, show g, show h, show i, show j, show k, show l, show m, show n, show o)

-- Insere os dados estatisticos da função stats numa string em no local de determinados caracteres especiais
inserestats [] _ = []
inserestats (z:t) x@(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) | z == '\161' = a++ inserestats t x
						   | z == '\162' = b++ inserestats t x
						   | z == '\163' = c++ inserestats t x
						   | z == '\8364' = d++ inserestats t x 
						   | z == '\165' = e++ inserestats t x 
						   | z == '\352' = f++ inserestats t x 
						   | z == '\167' = g++ inserestats t x 
						   | z == '\353' = h++ inserestats t x 
						   | z == '\169' = i++ inserestats t x 
						   | z == '\172' = j++ inserestats t x 
						   | z == '\176' = k++ inserestats t x 
						   | z == '\177' = l++ inserestats t x 
						   | z == '\178' = m++ inserestats t x 
						   | z == '\179' = n++ inserestats t x 
						   | z == '\181' = o++ inserestats t x 
						   | otherwise = z : inserestats t x

-- Faz a ligação entre a função stats e a inserestats
preparastats v ficheiro =do
				x <- stats ficheiro 
				return $ inserestats v x
-- Cria um ficheiro .tex com os dados estatisticos e após criar um .pdf abre-o		
escreveTexStats ficheiro = do
				pid0 <- runCommand "rm ./Temp/*"
				waitForProcess pid0
				x <- readFile "./Templates/modelostats.tex"
				y <- preparastats x ficheiro
				writeFile "./Temp/Estatisticas.tex" y
				pid <- runCommand "cp ./Templates/logo.png ./Temp/ ; cd Temp/; pdflatex Estatisticas.tex"
				waitForProcess pid
				pid2 <- runCommand "cd Temp/ ; mv Estatisticas.pdf ../Resultados"
				pid3 <- runCommand "cd .. ; gnome-open Estatisticas.pdf"
				waitForProcess pid3

------------------------------------------------------------------------         
-- CRIAÇÃO LATEX COM ESTATISTÍSTICAS MULTIDIMENSIONAIS
-- CRIAÇÃO LATEX COM ESTATISTÍSTICAS MULTIDIMENSIONAIS
-- CRIAÇÃO LATEX COM ESTATISTÍSTICAS MULTIDIMENSIONAIS
------------------------------------------------------------------------
procuraEmpregoPorCurso [] = ("Procura 1\186 emprego",0,0,0,0,0,0)
procuraEmpregoPorCurso ((a,b,c,d,e,f,g,h,i,j,k,l):t)
     | elem "Procura 1\\xc2\\xba emprego" j && c == "LEI" = (s,u+1,w,x,y,z,total +1)
     | elem "Procura 1\\xc2\\xba emprego" j && c == "LCC" = (s,u,w+1,x,y,z,total +1)
     | elem "Procura 1\\xc2\\xba emprego" j && c == "MI" = (s,u,w,x+1,y,z,total +1)
     | elem "Procura 1\\xc2\\xba emprego" j && c == "MEI" = (s,u,w,x,y+1,z,total +1)
     | elem "Procura 1\\xc2\\xba emprego" j && c == "MERSCOM" = (s,u,w,x,y,z+1,total +1)
     | otherwise = procuraEmpregoPorCurso t
        where (s,u,w,x,y,z,total) = procuraEmpregoPorCurso t


intTematicoPorCurso [] = ("Interesse pelas \225reas tem\225ticas",0,0,0,0,0,0)
intTematicoPorCurso ((a,b,c,d,e,f,g,h,i,j,k,l):t)
     | elem "Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas" j && c == "LEI" = (s,u+1,w,x,y,z,total+1)
     | elem "Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas" j && c == "LCC" = (s,u,w+1,x,y,z,total+1)
     | elem "Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas" j && c == "MI" = (s,u,w,x+1,y,z,total+1)
     | elem "Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas" j && c == "MEI" = (s,u,w,x,y+1,z,total+1)
     | elem "Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas" j && c == "MERSCOM" = (s,u,w,x,y,z+1,total+1)
     | otherwise = intTematicoPorCurso t
        where (s,u,w,x,y,z,total) = intTematicoPorCurso t


procuraProjPorCurso [] = ("Procura projecto/disserta\231\227o de mestrado",0,0,0,0,0,0)
procuraProjPorCurso ((a,b,c,d,e,f,g,h,i,j,k,l):t)
     | elem "Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado" j && c == "LEI" = (s,u+1,w,x,y,z,total+1)
     | elem "Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado" j && c == "LCC" = (s,u,w+1,x,y,z,total+1)
     | elem "Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado" j && c == "MI" = (s,u,w,x+1,y,z,total+1)
     | elem "Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado" j && c == "MEI" = (s,u,w,x,y+1,z,total+1)
     | elem "Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado" j && c == "MERSCOM" = (s,u,w,x,y,z+1,total+1)
     | otherwise = procuraProjPorCurso t
        where (s,u,w,x,y,z,total) = procuraProjPorCurso t


motivoPorCurso ficheiro = do
        x <- fimvalidaalunos ficheiro
        let y = ((procuraProjPorCurso x):(intTematicoPorCurso x):(procuraEmpregoPorCurso x):[])
            w = reverse ((somaTotais y):reverse y)
        z <- readFile "./Templates/tarefa4.tex"
        pid0 <- runCommand ("rm ./Temp/tarefa4.*")
        waitForProcess pid0
        writeFile ("./Temp/tarefa4.tex") (insereFicheiros (linhasTabela w) '*' z)
        pid1 <- runCommand ("cd Temp ; pdflatex tarefa4.tex ; cd ..")
        waitForProcess pid1


linhasTabela [] = []
linhasTabela ((a,b,c,d,e,f,total):t) = a ++ " & " ++ (show b) ++ " & " ++ (show c) ++ " & " ++ (show d) ++ " & " ++ (show e) ++ " & " ++ (show f) ++ " & " ++ show(total) ++ " \x5C\x5C \x5Chline \n" ++ (linhasTabela t)


insereFicheiros _ _ [] = []
insereFicheiros z y (x:xs) | x == y = z ++ (insereFicheiros z y xs)
                           | otherwise = x : (insereFicheiros z y xs)



somaTotais [] = ("Totais",0,0,0,0,0,0)
somaTotais ((a,b,c,d,e,f,total):t) = (u,b+v,c+w,d+x,e+y,f+z,total+subtotal)
        where (u,v,w,x,y,z,subtotal) = somaTotais t






------------------------------------------------------------------------         
-- MENU
-- MENU
-- MENU
-- MENU
------------------------------------------------------------------------

menu = do  putStrLn "\nEscolha uma opção: "; putStrLn "1) Fazer validação das inscrições";putStrLn "2) Criar Crachás"; putStrLn "3) Criar dados estatísticos";
            opcao <- getChar;
            case opcao of
                '1' -> do  putStrLn "\nIntroduza o caminho do ficheiro com as inscrições: ";
                            ficheiro <- getLine;
                            fim ficheiro;clearScreen;putStrLn "\nOperação Concluída com sucesso";
                            putStrLn "\nFicheiros criados: Alunos - Inscrições Válidas.txt, Universitários - Inscrições Válidas.txt, Empresas - Inscrições Válidas.txt, Inscrições Inválidas.txt na directoria 'Resultados'";
                            menu
                          
                '2' -> return ()
                
                '3' -> do putStrLn "\nIntroduza o caminho do ficheiro com as inscrições: ";
							ficheiro <- getLine;
                            escreveTexStats ficheiro;clearScreen;putStrLn "\nOperação Concluída com sucesso";
                            putStrLn "\nFicheiros criados: Estatisticas.tex, Estatisticas.pdf na directoria 'Resultados'";
                            menu
                
                _ -> do  putStrLn "\nPor favor escolha uma das opções abaixo indicadas";
                          menu
                        
          
{-
menu2 :: String -> IO ()
menu2 str = do { putStrLn "Escolha a opção: ";
                 op <- getChar;
                 case op of
                        '1' -> do { putStrLn ("Palavra" ++ str);
                                    menu
                                  }
                        '2' -> do { putStrLn ("Palavra invertida" ++ reverse str);
                                    menu
                                  }
                        _ -> do { putStrLn "Opção inválida!\n";
                                  menu2 str
                                }
                } -}




					
