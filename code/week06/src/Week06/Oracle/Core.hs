{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , oracleInst
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude

-- **************************
--          ON CHAIN
-- **************************

data Oracle = Oracle                                                  -- The Oracle is a paramaterized contract and the data type called Oracle is the parameter
    { oSymbol   :: !CurrencySymbol                                    -- First of four fields is the oSymbol which is the currency symbol of the NFT. Token Name is just empty string 
    , oOperator :: !PubKeyHash                                        -- oOperator is the owner of the Oracle and is the only one that can make updates where as anyone can use
    , oFee      :: !Integer                                           -- oFee is the fees in Lovelace that is required everytime someone uses the Oracle
    , oAsset    :: !AssetClass                                        -- is the target of the swap contract and in this case is the USD token 
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord) -- boiler plate code of type classes required to be serializable

PlutusTx.makeLift ''Oracle                                            -- boiler plate for liftable

data OracleRedeemer = Update | Use                                    -- here we define the redeemer to have two use cases: update and use 
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer                          -- use template haskell to implment IsData for the Oracle Redeemer data type 

{-# INLINABLE oracleTokenName #-}                                     -- starting from here are some helper definitions
oracleTokenName :: TokenName                                 
oracleTokenName = TokenName emptyByteString                           -- using emptyByteString for the token name 

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass                                   -- oracleAsset is used to uniquely identify the UTXO of the NFT with the Oracle value. Recall that AssetClass requires currency sym and tn 
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer   -- TxOut is the output of the UTXO that holds the Oracle. We want to look up the dataum and turn into an integer 
oracleValue o f = do                                                  -- do block is inside the Maybe Monad so the result of the bind can be nothing 
    dh      <- txOutDatum o                                           -- getting Datum from TxOut (can fail and result in nothing) or succeed in which case we get dh (datum hash)
    Datum d <- f dh                                                   -- f is the function we use to turn the dh into a Datum d
    PlutusTx.fromData d                                               -- Use the PlutusTx.fromData to maybe turn d into an integer 

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool    -- mkOracleValidator gets the parameter Oracle, Integer datum (the current exchange rate), redeemer type OracleRedeemer (use or get), ScriptContext and returns a Bool 
mkOracleValidator oracle x r ctx =                                                   -- x is the old Oracle value 
    traceIfFalse "token missing from input"  inputHasToken  &&                       -- checks to see if the input holds the NFT
    traceIfFalse "token missing from output" outputHasToken &&                       -- checks to see if the output holds the NFT 
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&    -- checks to see if the operator of the Oracle has signed withe the PubKeyHash from oOperator  
                  traceIfFalse "invalid output datum"       validOutputDatum                           -- checks that the outputDatum is an integer. since value is not checked operator can retreive fees
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&    -- checks to see if the Datum has changed. Use does not allow datum to change 
                  traceIfFalse "fees not paid"              feesPaid                                   -- checks if fee are paid 
  where
    info :: TxInfo                                       -- takes the context and extracts the TxInfo from it 
    info = scriptContextTxInfo ctx

    ownInput :: TxOut                                    -- the TxOut is the Oracle output that we are trying to consume 
    ownInput = case findOwnInput ctx of                  -- need to verify 
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool                                                               -- helper function to check if the NFT token is present. checks to see if Oracle input carries NFT
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1   -- assetClassValueOf :: Value -> AssetClass -> Integer how many coins of that Asset Class are contained in the value
                                                                                        -- should be only 1 coin for NFT 
                                                                                        -- txOutValue ownInput is the Value attached to the input that we are consuming

    ownOutput :: TxOut                                            -- checks to see if the use and update redeemers will produce exactly only one Oracle output 
    ownOutput = case getContinuingOutputs ctx of                  -- getContinuingOutputs retreives a list from the context of outputs going to Oracle address 
        [o] -> o                                                  -- checks to see if there is only one output 
        _   -> traceError "expected exactly one oracle output"    -- if there are zero or more than one outputs then return an error 

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1    -- checks to see if Oracle output carries the NFT

    outputDatum :: Maybe Integer                                  -- checks to see if it is a valid integer 
    outputDatum = oracleValue ownOutput (`findDatum` info)        -- findDatum takes the info and the datum hash and tries to look up the corresponding datum 

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum    -- checks that the outputDatum is not nothing.  If it is something then return true 

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput    -- check the value attached to the Oracle input 
        outVal = txOutValue ownOutput   -- check the value attached to the Oracle output 
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))    -- output should be at least as large as the input plus the fees. <> Semigroup operator to combine values. Ada.lovelaceValueOf to lovelace val

data Oracling                           -- boiler plate helper function to combine the Datum type and the Redeemer type 
instance Scripts.ScriptType Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.ScriptInstance Oracling    -- template Haskell used to compile into a script instance 
oracleInst oracle = Scripts.validator @Oracling            -- because it is parameterized the liftCode is necessary to lift it into the Plutus script 
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator    -- boiler plate code to turn it into a validator 
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address -- boiler plate code to turn validator into an script Oracle address 
oracleAddress = scriptAddress . oracleValidator




-- **************************
--          OFF CHAIN
-- **************************

-- Above code is for the on-chain part of the Oracle and the below code is the off-chain part of the Oracle 
-- Note that the Oracle provider only needs to provision the "update" function. The "use" function is not part of the Oracle off-chain code 

data OracleParams = OracleParams    -- parameters required to start the Oracle 
    { opFees   :: !Integer          -- fees that we want to charge
    , opSymbol :: !CurrencySymbol   -- CS of the Asset that we would like to swap Ada for 
    , opToken  :: !TokenName        -- TokenName of Asset that we would like to swap Ada for 
    } deriving (Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle -- startOracle only mints the NFT. To ensure Oracle has update price info, assigning value is left to update function 
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    -- show will get the string of the CurrencyError. pack converts the string into text. mapError is used for custom error messages. forgeContract is a function that mints various tokens of name and qty
    -- the CurrencyError type needs to be specified for "show"
    let cs     = Currency.currencySymbol osc    -- gets the currencySymbol cs from osc 
        oracle = Oracle
            { oSymbol   = cs                                     -- currencySymbol from above
            , oOperator = pkh                                    -- pkh from above 
            , oFee      = opFees op                              -- from OracleParams 
            , oAsset    = AssetClass (opSymbol op, opToken op)   -- from OracleParams 
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()  -- handles the case where Oracle just started has no UTXO, Oracle already started and has value 
updateOracle oracle x = do                                                                       -- Integer is the value we want to update the Oracle to 
    m <- findOracle oracle    -- helper fucntion that looks up an existing Oracle UTXO. fails if Oracle hasn't started and or not assigned value yet
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1    -- Constraint c is defined here by stating that tx must have output to be paid to a script address (mustPayToTheScript) 
                                                                                         -- providing the datum x and the NFT from the assetClassValue 
    case m of
        Nothing -> do         -- first case is started the Oracle but haven't provided the inital value yet. need to produce the first tx with the first output of Ada price in USD 
            ledgerTx <- submitTxConstraints (oracleInst oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do -- second case is we already have Oracle UTXO so we need to consume it first and then update datum. _ is used as we don't care about the old Datum. 
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>    -- creates a list of one key value pair of outputs we want to consume 
                          Constraints.scriptInstanceLookups (oracleInst oracle) <>    -- ??
                          Constraints.otherScript (oracleValidator oracle)            -- ??
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update) -- mustSpendScriptOutput because we first must consume the existing UTXO - it takes the update redeemer
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx    -- @Oracling to define the data types.  All fee payments to and from Oracle are handled automatically
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))    -- TxOutRef is the id of UTXO, TXOutTx is the UTXO, Integer is current exchange rate 
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)    -- utxoAt gets all the UTXO sitting at Oracle address. Map.filter creates a list of either 1 or 0 addresses with NFT 
    return $ case Map.toList utxos of    -- converts the map into a list of key value pairs with the Map.toList function 
        [(oref, o)] -> do                -- first case is we find one element 
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o    -- checks to see if the datum is valid and not corrupted. If ok then x will be the integer value of Datum 
                                                                                             -- txOutTxOut gets the TxOut from o. txOutTxTx o gives us a transaction
            return (oref, o, x)          -- if element is found then the triple is returned
        _           -> Nothing           -- second case is we find all other cases in which case we return nothing 
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1    -- Checks that the UTXO has only 1 NFT 

type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer    -- a helper function for Plutus Playground that combines the startOracle and updateOracle. update endpoint takes new value as param  

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op -- starts the Oracle by minting NFT only 
    tell $ Last $ Just oracle -- use tell to write the Oracle value because need to communicate to outside world since we don't know the value yet before run time 
                              -- Last Monoid remembers the Last Just value so that the current Ada price is always returned to users of Oracle 
    go oracle -- go starts the oracle and loops forever 
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update" -- blocks at update and as soon as an Integer (new price of Ada) is provided updateOracle is called and then go oracle is looped again
        updateOracle oracle x
        go oracle