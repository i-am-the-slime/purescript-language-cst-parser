module PureScript.CST.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Prim hiding (Row, Type)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.Argonaut.Decode.Generic (genericDecodeJson)

newtype ModuleName = ModuleName String

derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName
derive instance newtypeModuleName :: Newtype ModuleName _
derive instance Generic ModuleName _
instance EncodeJson ModuleName where
  encodeJson = genericEncodeJson
instance DecodeJson ModuleName where
  decodeJson = genericDecodeJson

type SourcePos =
  { line :: Int
  , column :: Int
  }

type SourceRange =
  { start :: SourcePos
  , end :: SourcePos
  }

data Comment l
  = Comment String
  | Space Int
  | Line l Int

derive instance Generic (Comment l) _
instance (EncodeJson l) => EncodeJson (Comment l) where
  encodeJson = genericEncodeJson
instance (DecodeJson l) => DecodeJson (Comment l) where
  decodeJson = genericDecodeJson

data LineFeed
  = LF
  | CRLF

derive instance Generic LineFeed _
instance EncodeJson LineFeed where
  encodeJson = genericEncodeJson
instance DecodeJson LineFeed where
  decodeJson = genericDecodeJson

data SourceStyle
  = ASCII
  | Unicode


derive instance eqSourceStyle :: Eq SourceStyle
derive instance Generic SourceStyle _
instance EncodeJson SourceStyle where
  encodeJson = genericEncodeJson
instance DecodeJson SourceStyle where
  decodeJson = genericDecodeJson

data IntValue
  = SmallInt Int
  | BigInt String
  | BigHex String

derive instance eqIntValue :: Eq IntValue
derive instance Generic IntValue _
instance EncodeJson IntValue where
  encodeJson = genericEncodeJson
instance DecodeJson IntValue where
  decodeJson = genericDecodeJson

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftArrow SourceStyle
  | TokRightArrow SourceStyle
  | TokRightFatArrow SourceStyle
  | TokDoubleColon SourceStyle
  | TokForall SourceStyle
  | TokEquals
  | TokPipe
  | TokTick
  | TokDot
  | TokComma
  | TokUnderscore
  | TokBackslash
  | TokAt
  | TokLowerName (Maybe ModuleName) String
  | TokUpperName (Maybe ModuleName) String
  | TokOperator (Maybe ModuleName) String
  | TokSymbolName (Maybe ModuleName) String
  | TokSymbolArrow SourceStyle
  | TokHole String
  | TokChar String Char
  | TokString String String
  | TokRawString String
  | TokInt String IntValue
  | TokNumber String Number
  | TokLayoutStart Int
  | TokLayoutSep Int
  | TokLayoutEnd Int

derive instance eqToken :: Eq Token
derive instance Generic Token _
instance EncodeJson Token where
  encodeJson = genericEncodeJson
instance DecodeJson Token where
  decodeJson = genericDecodeJson

type SourceToken =
  { range :: SourceRange
  , leadingComments :: Array (Comment LineFeed)
  , trailingComments :: Array (Comment Void)
  , value :: Token
  }

newtype Ident = Ident String

derive newtype instance eqIdent :: Eq Ident
derive newtype instance ordIdent :: Ord Ident
derive instance newtypeIdent :: Newtype Ident _
derive newtype instance EncodeJson Ident
derive newtype instance DecodeJson Ident

newtype Proper = Proper String

derive newtype instance eqProper :: Eq Proper
derive newtype instance ordProper :: Ord Proper
derive instance newtypeProper :: Newtype Proper _
derive newtype instance EncodeJson Proper
derive newtype instance DecodeJson Proper

newtype Label = Label String

derive newtype instance eqLabel :: Eq Label
derive newtype instance ordLabel :: Ord Label
derive instance newtypeLabel :: Newtype Label _
derive newtype instance EncodeJson Label
derive newtype instance DecodeJson Label

newtype Operator = Operator String

derive newtype instance eqOperator :: Eq Operator
derive newtype instance ordOperator :: Ord Operator
derive instance newtypeOperator :: Newtype Operator _
derive newtype instance EncodeJson Operator
derive newtype instance DecodeJson Operator

newtype Name a = Name
  { token :: SourceToken
  , name :: a
  }

derive instance newtypeName :: Newtype (Name a) _
derive newtype instance EncodeJson a => EncodeJson (Name a)
derive newtype instance DecodeJsonField a => DecodeJson (Name a)

newtype QualifiedName a = QualifiedName
  { token :: SourceToken
  , module :: Maybe ModuleName
  , name :: a
  }

derive instance newtypeQualifiedName :: Newtype (QualifiedName a) _
derive newtype instance EncodeJson a => EncodeJson (QualifiedName a)
derive newtype instance DecodeJsonField a => DecodeJson (QualifiedName a)

newtype Wrapped a = Wrapped
  { open :: SourceToken
  , value :: a
  , close :: SourceToken
  }

derive instance newtypeWrapped :: Newtype (Wrapped a) _
derive newtype instance EncodeJson a => EncodeJson (Wrapped a)
derive newtype instance DecodeJsonField a => DecodeJson (Wrapped a)

newtype Separated a = Separated
  { head :: a
  , tail :: Array (Tuple SourceToken a)
  }

derive instance newtypeSeparated :: Newtype (Separated a) _
derive newtype instance EncodeJson a => EncodeJson (Separated a)
derive newtype instance (DecodeJsonField a, DecodeJson a) => DecodeJson (Separated a)

newtype Labeled a b = Labeled
  { label :: a
  , separator :: SourceToken
  , value :: b
  }

derive instance newtypeLabeled :: Newtype (Labeled a b) _
derive newtype instance (EncodeJson a, EncodeJson b) => EncodeJson (Labeled a b)
derive newtype instance (DecodeJsonField a, DecodeJsonField b, DecodeJson a, DecodeJson b) => DecodeJson (Labeled a b)

newtype Prefixed a = Prefixed
  { prefix :: Maybe SourceToken
  , value :: a
  }

derive instance newtypePrefixed :: Newtype (Prefixed a) _
derive newtype instance EncodeJson a => EncodeJson (Prefixed a)
derive newtype instance (DecodeJsonField a, DecodeJson a) => DecodeJson (Prefixed a)

type Delimited a = Wrapped (Maybe (Separated a))
type DelimitedNonEmpty a = Wrapped (Separated a)

data OneOrDelimited a
  = One a
  | Many (DelimitedNonEmpty a)

derive instance Generic (OneOrDelimited a) _
instance (EncodeJson a) => EncodeJson (OneOrDelimited a) where
  encodeJson x = genericEncodeJson x
instance (DecodeJson a, DecodeJsonField a) => DecodeJson (OneOrDelimited a) where
  decodeJson x = genericDecodeJson x

data Type e
  = TypeVar (Name Ident)
  | TypeConstructor (QualifiedName Proper)
  | TypeWildcard SourceToken
  | TypeHole (Name Ident)
  | TypeString SourceToken String
  | TypeInt (Maybe SourceToken) SourceToken IntValue
  | TypeRow (Wrapped (Row e))
  | TypeRecord (Wrapped (Row e))
  | TypeForall SourceToken (NonEmptyArray (TypeVarBinding (Prefixed (Name Ident)) e)) SourceToken (Type e)
  | TypeKinded (Type e) SourceToken (Type e)
  | TypeApp (Type e) (NonEmptyArray (Type e))
  | TypeOp (Type e) (NonEmptyArray (Tuple (QualifiedName Operator) (Type e)))
  | TypeOpName (QualifiedName Operator)
  | TypeArrow (Type e) SourceToken (Type e)
  | TypeArrowName SourceToken
  | TypeConstrained (Type e) SourceToken (Type e)
  | TypeParens (Wrapped (Type e))
  | TypeError e

-- TODO
derive instance Generic (Type e) _
instance (EncodeJson e, EncodeJson (Row e)) => EncodeJson (Type e) where
  encodeJson x = genericEncodeJson x
instance (DecodeJson e, DecodeJsonField e) => DecodeJson (Type e) where
  decodeJson x = genericDecodeJson x

data TypeVarBinding a e
  = TypeVarKinded (Wrapped (Labeled a (Type e)))
  | TypeVarName a

derive instance Generic (TypeVarBinding a e) _
instance (EncodeJson a, EncodeJson e) => EncodeJson (TypeVarBinding a e) where
  encodeJson x = genericEncodeJson x
instance (DecodeJson a, DecodeJsonField a, DecodeJson e, DecodeJsonField e) => DecodeJson (TypeVarBinding a e) where
  decodeJson x = genericDecodeJson x

newtype Row e = Row
  { labels :: Maybe (Separated (Labeled (Name Label) (Type e)))
  , tail :: Maybe (Tuple SourceToken (Type e))
  }

derive instance newtypeRow :: Newtype (Row e) _
derive newtype instance EncodeJson e => EncodeJson (Row e)
derive newtype instance (DecodeJsonField e, DecodeJson e) => DecodeJson (Row e)

newtype Module e = Module
  { header :: ModuleHeader e
  , body :: ModuleBody e
  }

derive instance newtypeModule :: Newtype (Module e) _

newtype ModuleHeader e = ModuleHeader
  { keyword :: SourceToken
  , name :: Name ModuleName
  , exports :: Maybe (DelimitedNonEmpty (Export e))
  , where :: SourceToken
  , imports :: Array (ImportDecl e)
  }

derive instance newtypeModuleHeader :: Newtype (ModuleHeader e) _

newtype ModuleBody e = ModuleBody
  { decls :: Array (Declaration e)
  , trailingComments :: Array (Comment LineFeed)
  , end :: SourcePos
  }

derive instance newtypeModuleBody :: Newtype (ModuleBody e) _

data Export e
  = ExportValue (Name Ident)
  | ExportOp (Name Operator)
  | ExportType (Name Proper) (Maybe DataMembers)
  | ExportTypeOp SourceToken (Name Operator)
  | ExportClass SourceToken (Name Proper)
  | ExportModule SourceToken (Name ModuleName)
  | ExportError e



data DataMembers
  = DataAll SourceToken
  | DataEnumerated (Delimited (Name Proper))

data Declaration e
  = DeclData (DataHead e) (Maybe (Tuple SourceToken (Separated (DataCtor e))))
  | DeclType (DataHead e) SourceToken (Type e)
  | DeclNewtype (DataHead e) SourceToken (Name Proper) (Type e)
  | DeclClass (ClassHead e) (Maybe (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (Type e)))))
  | DeclInstanceChain (Separated (Instance e))
  | DeclDerive SourceToken (Maybe SourceToken) (InstanceHead e)
  | DeclKindSignature SourceToken (Labeled (Name Proper) (Type e))
  | DeclSignature (Labeled (Name Ident) (Type e))
  | DeclValue (ValueBindingFields e)
  | DeclFixity FixityFields
  | DeclForeign SourceToken SourceToken (Foreign e)
  | DeclRole SourceToken SourceToken (Name Proper) (NonEmptyArray (Tuple SourceToken Role))
  | DeclError e

newtype Instance e = Instance
  { head :: InstanceHead e
  , body :: Maybe (Tuple SourceToken (NonEmptyArray (InstanceBinding e)))
  }

derive instance newtypeInstance :: Newtype (Instance e) _

data InstanceBinding e
  = InstanceBindingSignature (Labeled (Name Ident) (Type e))
  | InstanceBindingName (ValueBindingFields e)

newtype ImportDecl e = ImportDecl
  { keyword :: SourceToken
  , module :: Name ModuleName
  , names :: Maybe (Tuple (Maybe SourceToken) (DelimitedNonEmpty (Import e)))
  , qualified :: Maybe (Tuple SourceToken (Name ModuleName))
  }

derive instance newtypeImportDecl :: Newtype (ImportDecl e) _

data Import e
  = ImportValue (Name Ident)
  | ImportOp (Name Operator)
  | ImportType (Name Proper) (Maybe DataMembers)
  | ImportTypeOp SourceToken (Name Operator)
  | ImportClass SourceToken (Name Proper)
  | ImportError e

type DataHead e =
  { keyword :: SourceToken
  , name :: Name Proper
  , vars :: Array (TypeVarBinding (Name Ident) e)
  }

newtype DataCtor e = DataCtor
  { name :: Name Proper
  , fields :: Array (Type e)
  }

derive instance newtypeDataCtor :: Newtype (DataCtor e) _

type ClassHead e =
  { keyword :: SourceToken
  , super :: Maybe (Tuple (OneOrDelimited (Type e)) SourceToken)
  , name :: Name Proper
  , vars :: Array (TypeVarBinding (Name Ident) e)
  , fundeps :: Maybe (Tuple SourceToken (Separated ClassFundep))
  }

data ClassFundep
  = FundepDetermined SourceToken (NonEmptyArray (Name Ident))
  | FundepDetermines (NonEmptyArray (Name Ident)) SourceToken (NonEmptyArray (Name Ident))

type InstanceHead e =
  { keyword :: SourceToken
  , name :: Maybe (Tuple (Name Ident) SourceToken)
  , constraints :: Maybe (Tuple (OneOrDelimited (Type e)) SourceToken)
  , className :: QualifiedName Proper
  , types :: Array (Type e)
  }

data Fixity
  = Infix
  | Infixl
  | Infixr

data FixityOp
  = FixityValue (QualifiedName (Either Ident Proper)) SourceToken (Name Operator)
  | FixityType SourceToken (QualifiedName Proper) SourceToken (Name Operator)

type FixityFields =
  { keyword :: Tuple SourceToken Fixity
  , prec :: Tuple SourceToken Int
  , operator :: FixityOp
  }

type ValueBindingFields e =
  { name :: Name Ident
  , binders :: Array (Binder e)
  , guarded :: Guarded e
  }

data Guarded e
  = Unconditional SourceToken (Where e)
  | Guarded (NonEmptyArray (GuardedExpr e))

derive instance Generic (Guarded e) _
instance (EncodeJson e) => EncodeJson (Guarded e) where
  encodeJson = genericEncodeJson
instance (DecodeJson e, DecodeJsonField e) => DecodeJson (Guarded e) where
  decodeJson = genericDecodeJson

newtype GuardedExpr e = GuardedExpr
  { bar :: SourceToken
  , patterns :: Separated (PatternGuard e)
  , separator :: SourceToken
  , where :: Where e
  }

derive instance newtypeGuardedExpr :: Newtype (GuardedExpr e) _
derive newtype instance EncodeJson e => EncodeJson (GuardedExpr e)
derive newtype instance (DecodeJsonField e, DecodeJson e) => DecodeJson (GuardedExpr e)

newtype PatternGuard e = PatternGuard
  { binder :: Maybe (Tuple (Binder e) SourceToken)
  , expr :: Expr e
  }

derive instance newtypePatternGuard :: Newtype (PatternGuard e) _
derive newtype instance EncodeJson e => EncodeJson (PatternGuard e)
derive newtype instance (DecodeJsonField e, DecodeJson e) => DecodeJson (PatternGuard e)


data Foreign e
  = ForeignValue (Labeled (Name Ident) (Type e))
  | ForeignData SourceToken (Labeled (Name Proper) (Type e))
  | ForeignKind SourceToken (Name Proper)

data Role
  = Nominal
  | Representational
  | Phantom

derive instance Generic Role _
instance EncodeJson Role where
  encodeJson = genericEncodeJson
instance DecodeJson Role where
  decodeJson = genericDecodeJson

data Expr e
  = ExprHole (Name Ident)
  | ExprSection SourceToken
  | ExprIdent (QualifiedName Ident)
  | ExprConstructor (QualifiedName Proper)
  | ExprBoolean SourceToken Boolean
  | ExprChar SourceToken Char
  | ExprString SourceToken String
  | ExprInt SourceToken IntValue
  | ExprNumber SourceToken Number
  | ExprArray (Delimited (Expr e))
  | ExprRecord (Delimited (RecordLabeled (Expr e)))
  | ExprParens (Wrapped (Expr e))
  | ExprTyped (Expr e) SourceToken (Type e)
  | ExprInfix (Expr e) (NonEmptyArray (Tuple (Wrapped (Expr e)) (Expr e)))
  | ExprOp (Expr e) (NonEmptyArray (Tuple (QualifiedName Operator) (Expr e)))
  | ExprOpName (QualifiedName Operator)
  | ExprNegate SourceToken (Expr e)
  | ExprRecordAccessor (RecordAccessor e)
  | ExprRecordUpdate (Expr e) (DelimitedNonEmpty (RecordUpdate e))
  | ExprApp (Expr e) (NonEmptyArray (AppSpine Expr e))
  | ExprLambda (Lambda e)
  | ExprIf (IfThenElse e)
  | ExprCase (CaseOf e)
  | ExprLet (LetIn e)
  | ExprDo (DoBlock e)
  | ExprAdo (AdoBlock e)
  | ExprError e

derive instance Generic (Expr e) _
instance (EncodeJson e) => EncodeJson (Expr e) where
  encodeJson = genericEncodeJson
instance (DecodeJson e, DecodeJsonField e) => DecodeJson (Expr e) where
  decodeJson = genericDecodeJson

data AppSpine f e
  = AppType SourceToken (Type e)
  | AppTerm (f e)

derive instance Generic (AppSpine f e) _
instance (EncodeJson e, EncodeJson (f e)) => EncodeJson (AppSpine f e) where
  encodeJson = genericEncodeJson
instance (DecodeJson e, DecodeJsonField e, DecodeJson (f e)) => DecodeJson (AppSpine f e) where
  decodeJson = genericDecodeJson

data RecordLabeled a
  = RecordPun (Name Ident)
  | RecordField (Name Label) SourceToken a

derive instance Generic (RecordLabeled a) _
instance (EncodeJson a) => EncodeJson (RecordLabeled a) where
  encodeJson = genericEncodeJson
instance (DecodeJson a) => DecodeJson (RecordLabeled a) where
  decodeJson = genericDecodeJson

data RecordUpdate e
  = RecordUpdateLeaf (Name Label) SourceToken (Expr e)
  | RecordUpdateBranch (Name Label) (DelimitedNonEmpty (RecordUpdate e))

derive instance Generic (RecordUpdate e) _
instance (EncodeJson e) => EncodeJson (RecordUpdate e) where
  encodeJson = genericEncodeJson
instance (DecodeJson e, DecodeJsonField e) => DecodeJson (RecordUpdate e) where
  decodeJson = genericDecodeJson

type RecordAccessor e =
  { expr :: Expr e
  , dot :: SourceToken
  , path :: Separated (Name Label)
  }

type Lambda e =
  { symbol :: SourceToken
  , binders :: NonEmptyArray (Binder e)
  , arrow :: SourceToken
  , body :: Expr e
  }

type IfThenElse e =
  { keyword :: SourceToken
  , cond :: Expr e
  , then :: SourceToken
  , true :: Expr e
  , else :: SourceToken
  , false :: Expr e
  }

type CaseOf e =
  { keyword :: SourceToken
  , head :: Separated (Expr e)
  , of :: SourceToken
  , branches :: NonEmptyArray (Tuple (Separated (Binder e)) (Guarded e))
  }

type LetIn e =
  { keyword :: SourceToken
  , bindings :: NonEmptyArray (LetBinding e)
  , in :: SourceToken
  , body :: Expr e
  }

newtype Where e = Where
  { expr :: Expr e
  , bindings :: Maybe (Tuple SourceToken (NonEmptyArray (LetBinding e)))
  }

derive instance newtypeWhere :: Newtype (Where e) _
derive newtype instance EncodeJson e => EncodeJson (Where e)
derive newtype instance (DecodeJsonField e, DecodeJson e) => DecodeJson (Where e)

data LetBinding e
  = LetBindingSignature (Labeled (Name Ident) (Type e))
  | LetBindingName (ValueBindingFields e)
  | LetBindingPattern (Binder e) SourceToken (Where e)
  | LetBindingError e

derive instance Generic (LetBinding e) _
instance (EncodeJson e) => EncodeJson (LetBinding e) where
  encodeJson = genericEncodeJson
instance (DecodeJson e, DecodeJsonField e) => DecodeJson (LetBinding e) where
  decodeJson = genericDecodeJson

type DoBlock e =
  { keyword :: SourceToken
  , statements :: NonEmptyArray (DoStatement e)
  }

data DoStatement e
  = DoLet SourceToken (NonEmptyArray (LetBinding e))
  | DoDiscard (Expr e)
  | DoBind (Binder e) SourceToken (Expr e)
  | DoError e

derive instance Generic (DoStatement e) _
instance (EncodeJson e) => EncodeJson (DoStatement e) where
  encodeJson = genericEncodeJson
instance (DecodeJson e, DecodeJsonField e) => DecodeJson (DoStatement e) where
  decodeJson = genericDecodeJson

type AdoBlock e =
  { keyword :: SourceToken
  , statements :: Array (DoStatement e)
  , in :: SourceToken
  , result :: Expr e
  }

data Binder e
  = BinderWildcard SourceToken
  | BinderVar (Name Ident)
  | BinderNamed (Name Ident) SourceToken (Binder e)
  | BinderConstructor (QualifiedName Proper) (Array (Binder e))
  | BinderBoolean SourceToken Boolean
  | BinderChar SourceToken Char
  | BinderString SourceToken String
  | BinderInt (Maybe SourceToken) SourceToken IntValue
  | BinderNumber (Maybe SourceToken) SourceToken Number
  | BinderArray (Delimited (Binder e))
  | BinderRecord (Delimited (RecordLabeled (Binder e)))
  | BinderParens (Wrapped (Binder e))
  | BinderTyped (Binder e) SourceToken (Type e)
  | BinderOp (Binder e) (NonEmptyArray (Tuple (QualifiedName Operator) (Binder e)))
  | BinderError e

derive instance Generic (Binder e) _
instance (EncodeJson e) => EncodeJson (Binder e) where
  encodeJson = genericEncodeJson
instance (DecodeJson e, DecodeJsonField e) => DecodeJson (Binder e) where
  decodeJson = genericDecodeJson