module Propellor.Property.Gpg where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt

import System.PosixCompat

installed :: Property DebianLike
installed = Apt.installed ["gnupg"]

-- A numeric id, or a description of the key, in a form understood by gpg.
newtype GpgKeyId = GpgKeyId { getGpgKeyId :: String }

data GpgKeyType = GpgPubKey | GpgPrivKey

-- | Sets up a user with a gpg key from the privdata.
--
-- Note that if a secret key is exported using gpg -a --export-secret-key,
-- the public key is also included. Or just a public key could be
-- exported, and this would set it up just as well.
--
-- Recommend only using this for low-value dedicated role keys.
-- No attempt has been made to scrub the key out of memory once it's used.
keyImported :: GpgKeyId -> User -> Property HasInfo
keyImported key@(GpgKeyId keyid) user@(User u) = prop
	`requires` installed
  where
	desc = u ++ " has gpg key " ++ show keyid
	prop = withPrivData src (Context keyid) $ \getkey ->
		property desc $ getkey $ \key' -> do
			let keylines = privDataLines key'
			ifM (liftIO $ hasGpgKey (parse keylines))
				(return NoChange
				, makeChange $ withHandle StdinHandle createProcessSuccess
					(proc "su" ["-c", "gpg --import", u]) $ \h -> do
						fileEncoding h
						hPutStr h (unlines keylines)
						hClose h
				)
	src = PrivDataSource GpgKey "Either a gpg public key, exported with gpg --export -a, or a gpg private key, exported with gpg --export-secret-key -a"

	parse ("-----BEGIN PGP PUBLIC KEY BLOCK-----":_) = Just GpgPubKey
	parse ("-----BEGIN PGP PRIVATE KEY BLOCK-----":_) = Just GpgPrivKey
	parse _ = Nothing

	hasGpgKey Nothing = error $ "Failed to run gpg parser on armored key " ++ keyid
	hasGpgKey (Just GpgPubKey) = hasPubKey key user
	hasGpgKey (Just GpgPrivKey) = hasPrivKey key user

dotDir :: User -> IO FilePath
dotDir (User u) = do
	home <- homeDirectory <$> getUserEntryForName u
	return $ home </> ".gnupg"

hasPrivKey :: GpgKeyId -> User -> IO Bool
hasPrivKey (GpgKeyId keyid) (User u) = catchBoolIO $
	snd <$> processTranscript "su" ["-c", "gpg --list-secret-keys", keyid, u] Nothing

hasPubKey :: GpgKeyId -> User -> IO Bool
hasPubKey (GpgKeyId keyid) (User u) = catchBoolIO $
	snd <$> processTranscript "su" ["-c", "gpg --list-public-keys", keyid, u] Nothing
