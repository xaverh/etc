#!/usr/bin/lua
password_length = 13

numbers = { ["0"] = 0, ["1"] = 12309, ["2"] = 17, ["3"] = 123123, ["4"] = -123, ["5"] = 1001, ["6"] = 6, ["7"] = 8, ["8"] = 32, ["9"] = 1023, a = 723, b = 234, c = 123, d = 9, e = 0, f = 1233, g = 989, h = 1321, i = 90, j = 222222, k = 65432, l = 909, m = 1, n = 2, o = 747, p = -3, q = 12, r = 12, s = 945, t = 34, u = 12, v = -33, w = 0, x = 99999, y = -2, z = 123, A = 9, B = 321, C = 99, D = 9912, E = 8289, F = 12893, G = 1983, H = 1023, I = 989, J = 900, K = 898, L = 0, M = -1010, N = 9129, O = 112111, P = 10, Q = 0, R = 999, S = 100, T = 1230, U = -658, V = 100000, W = 1000000, X = 10000000, Y = 100000000, Z = 314159837253, ["!"] = 893, ['"'] = 271, ["#"] = 5, ["$"] = 4, ["%"] = 122, ["&"] = 9, ["'"] = 9, ["("] = 12, [")"] = -1234, ["*"] = -7836, ["+"] = 888, [","] = 12399, ["-"] = 99, ["."] = 91923, ["/"] = 181, [":"] = 99, [";"] = 1112221, ["<"] = 0, ["="] = -2, [">"] = 12, ["?"] = 21, ["@"] = 11, ["["] = 12332, ["]"] = 112, ["\\"] = 1122, ["^"] = 100, ["_"] = 101, ["{"] = 102, ["}"] = -103, ["|"] = -104, ["~"] = -1099, ["`"] = 97612, [" "] = 128338 }

print "Please enter encrypting passphrase"
passphrase = string.sub(io.read(),1,password_length)
print "Please enter passwort purpose"
purpose = string.sub(io.read(),1,password_length)
print [[Do you want to have special characters (e.g. /,$%&'?=[] etc.) in your password? (Y/n)]]
sc = io.read()
special_characters = sc ~= "n" and sc ~= "N"

if special_characters then
    letters = {"!","#","$","%","&","'","(",")","*","+",",","-",".","/","0","1","2","3","4","5","6","7","8","9",":",";","<","=",">","?","@","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","[","\\","]","_","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","{","|","}","~"}
else
    letters = {"0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"}
end

-- #salt > 2 * #password
salt = [[@w4`$e t543jjs Der Text muss etwas l"@nger weRden]]

function add_salt (string, salt, target_length)
    if target_length <= #string then
        return string
    else
        return string .. string.sub(salt, 1, target_length - #string)
    end
end

function string_to_table (s)
    local t = {}
    for i = 1, #s do
        table.insert(t, numbers[string.sub(s, i, i)] or numbers[string.sub(salt,i+10,i+10)]) -- replacing non-ASCII characters with characters from the salt string
    end
    return t
end

function table_xor (t1, t2)
    local xor = {}
    for k, v in ipairs(t1) do
        table.insert(xor, v ~ t2[k])
    end
    return xor
end

passphrase = add_salt(passphrase, salt, password_length)
purpose = string.reverse(add_salt(purpose, string.reverse(salt), password_length))

--[[ For testing:
print(#(string_to_table(passphrase)))
print(#(string_to_table(purpose)))
print(#letters)
--]]

password = {}
for k, v in ipairs(table_xor(string_to_table(passphrase),string_to_table(purpose))) do
    table.insert(password, letters[v % #letters + 1])
end
print(table.concat(password))

print "Press any key to close..."
io.read()