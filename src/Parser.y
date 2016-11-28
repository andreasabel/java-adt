{
module Parser where

import Syntax
import Lexer
}

%name parser Decls
%tokentype { Token }


%token
    id      { TkId $$       }
    '::'    { TkColon       }
    data    { TkData        }
    visitor { TkVisitor     }
    '|'     { TkBar         }
    '{'     { TkOpenBrace   }
    '}'     { TkCloseBrace  }
    '['     { TkOpenSquare  }
    ']'     { TkCloseSquare }
    '('     { TkOpenParen   }
    ')'     { TkCloseParen  }
    '='     { TkEqual       }
    ','     { TkComma       }

%%

Decls : MaybeDefs               { $1 }

MaybeDefs
    :                          { [] }
    | Def MaybeDefs            { $1 : $2 }

Def : data id Params '=' Constrs Visitors   { Data $2 $3 $5 $6 }

Params
    :                          { [] }
    | id Params                { $1 : $2}

-- no empty types

Constrs
    : Constr '|' Constrs       { $1 : $3 }
    | Constr                   { [$1] }

Constr
    : id Fields                { Constructor $1 $2 }
    | id                       { Constructor $1 [] }

Fields : '{' FieldList '}'     { $2 }

FieldList
    : id '::' Type ',' FieldList { Field $1 $3 : $5 }
    | id '::' Type               { [Field $1 $3] }

Type
    : Type Atom                { App $1 $2 }
    | Atom                     { $1 }

Atom
    : id                       { Name $1 }
    | '[' Type ']'             { List $2 }
    | '(' Type ')'             { $2 }

Visitors
    : visitor id id Visitors   { Visitor $3 (TypeId $2) : $4 }
    |                          { [] }

{

happyError = error "Parse error"

}
