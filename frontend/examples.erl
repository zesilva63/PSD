
% SYSTEM MESSAGES DEFINITION



% REGISTER

% pedido de registo
#'Message'{type = "REGISTER", clientData = #'ClientData'{ username = "the63x", password = "pass"}, request = undefined, response = undefined}
% resposta a registo com sucesso
#'Message'{type = "RESPONSE", clientData = undefined, request = undefined, response = #'Response'{result = "OK", description = "You are now registered with sucess."}}
% resposta a registo com erro user exists
#'Message'{type = "RESPONSE", clientData = undefined, request = undefined, response = #'Response'{result = "ERROR", description = "User already exists in the system."}}


% LOGIN

% pedido de login
#'Message'{type = "LOGIN", clientData = #'ClientData'{ username = "the63x", password = "pass" }, request = undefined, response = undefined}
% resposta a login com sucesso
#'Message'{type = "RESPONSE", clientData = undefined, request = undefined, response = #'Response'{result = "OK", description = "You are now logged in."}}
% resposta a login com erro 
#'Message'{type = "RESPONSE", clientData = undefined, request = undefined, response = #'Response'{result = "ERROR", description = "Wrong username or password."}}


% SELL 

#'Message'{type = "BUY", clientData = #'ClientData'{ username = "the63x", password = "pass" }, request = undefined, response = undefined}


% BUY