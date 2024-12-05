module TSVReader (readTSVFile) where

-- Function to read entire TSV file as a list of strings
readTSVFile :: FilePath -> IO [String]
readTSVFile filePath = do
    contents <- readFile filePath
    return (lines contents)


