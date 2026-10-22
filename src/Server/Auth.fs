/// Password hashing (PBKDF2) and opaque session tokens.
module Auth

open System
open System.Security.Cryptography
open System.Text

let private iterations = 210_000
let private algorithm = HashAlgorithmName.SHA256

let hashPassword (password: string) =
    let salt = RandomNumberGenerator.GetBytes 16
    let key = Rfc2898DeriveBytes.Pbkdf2(password, salt, iterations, algorithm, 32)
    $"pbkdf2-sha256${iterations}${Convert.ToBase64String salt}${Convert.ToBase64String key}"

let verifyPassword (password: string) (stored: string) =
    match stored.Split '$' with
    | [| "pbkdf2-sha256"; iter; salt; key |] ->
        let expected = Convert.FromBase64String key
        let actual =
            Rfc2898DeriveBytes.Pbkdf2(password, Convert.FromBase64String salt, int iter, algorithm, expected.Length)

        CryptographicOperations.FixedTimeEquals(actual, expected)
    | _ -> false

/// A fresh random token for the client; only its hash is stored server-side.
let newToken () =
    Convert.ToBase64String(RandomNumberGenerator.GetBytes 32).TrimEnd('=').Replace('+', '-').Replace('/', '_')

let hashToken (token: string) =
    SHA256.HashData(Encoding.UTF8.GetBytes token) |> Convert.ToHexString

/// Reads the token from an `Authorization: Bearer <token>` header value.
let bearerToken (header: string) =
    if not (isNull header) && header.StartsWith("Bearer ", StringComparison.OrdinalIgnoreCase) then
        Some(header.Substring(7).Trim())
    else
        None
