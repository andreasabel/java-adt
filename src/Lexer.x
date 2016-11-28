{
module Lexer where
}

%wrapper "basic"

$digit      = 0-9
$alpha      = [ A-Z a-z ]

tokens :-

   $white+                         ;
   "--"[^v].*                      ;
   "--"v[^i].*                     ;
   "--"vi[^s].*                    ;
   "--"vis[^i].*                   ;
   "--"visi[^t].*                  ;
   "--"visit[^o].*                 ;
   "--"visito[^r].*                ;
   "--visitor"                     { \ _ -> TkVisitor     }

   data                            { \ _ -> TkData        }
   \|                              { \ _ -> TkBar         }
   \,                              { \ _ -> TkComma       }
   \;                              { \ _ -> TkSemi        }
   \:\:                            { \ _ -> TkColon       }
   \=                              { \ _ -> TkEqual       }
   \{                              { \ _ -> TkOpenBrace   }
   \}                              { \ _ -> TkCloseBrace  }
   \[                              { \ _ -> TkOpenSquare  }
   \]                              { \ _ -> TkCloseSquare }
   \(                              { \ _ -> TkOpenParen   }
   \)                              { \ _ -> TkCloseParen  }

   [$alpha $digit \_ \' \< \>]+    { TkId }

{

data Token
  = TkId String
  | TkData       | TkVisitor
  | TkOpenBrace  | TkCloseBrace
  | TkOpenSquare | TkCloseSquare
  | TkOpenParen  | TkCloseParen
  | TkEqual      | TkColon
  | TkComma      | TkSemi
  | TkBar
  | TkEOF
    deriving Show

}
