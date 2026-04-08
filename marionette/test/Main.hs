{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket, throwIO)
import Control.Monad.Catch ()
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Lucid
import Network.HTTP.Types (status200)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Process.Typed
    ( checkExitCode
    , nullStream
    , proc
    , setStderr
    , setStdout
    , startProcess
    )
import Test.Hspec hiding (after, before)
import Test.Marionette
import Test.Marionette qualified as Marionette
import Test.Marionette.Protocol (Error)
import UnliftIO.Exception (catch)
import Prelude hiding (print)

page :: Text -> Html () -> Html ()
page title body =
    html_ do
        head_ $ title_ (toHtml title)
        body_ body

app :: Application
app req respond =
    respond . responseLBS status200 [("Content-Type", "text/html")] . renderBS $
        case rawPathInfo req of
            "/form" ->
                page "Form Page" do
                    form_ do
                        input_ [id_ "text-input", type_ "text", name_ "q"]
                        input_ [id_ "checkbox", type_ "checkbox", name_ "check"]
                        select_ [id_ "select"] do
                            option_ [value_ "a"] "Option A"
                            option_ [value_ "b"] "Option B"
                        button_ [id_ "submit", type_ "submit"] "Submit"
            "/link" ->
                page "Link Page" do
                    a_ [id_ "link", href_ "/target"] "Click me"
                    a_ [id_ "partial", href_ "/target"] "Click here for more"
            "/target" ->
                page "Target Page" do
                    p_ [id_ "result"] "You arrived!"
            "/shadow" ->
                page "Shadow DOM Page" do
                    div_ [id_ "host"] mempty
                    script_
                        "\
                        \const host = document.getElementById('host');\
                        \const shadow = host.attachShadow({mode: 'open'});\
                        \shadow.innerHTML = '<p id=\"shadow-p\">Shadow content</p>';"
            "/alert" ->
                page "Alert Page" do
                    button_ [id_ "alert-btn", onclick_ "alert('Hello!')"] "Alert"
                    button_ [id_ "confirm-btn", onclick_ "confirm('Sure?')"] "Confirm"
                    button_ [id_ "prompt-btn", onclick_ "prompt('Input:')"] "Prompt"
            _ ->
                page "Marionette Test" do
                    h1_ [id_ "heading"] "Hello, Marionette!"
                    p_ [id_ "para", class_ "content"] "Test paragraph"
                    p_ [id_ "para2", class_ "content"] "Second paragraph"
                    div_ [id_ "parent"] do
                        span_ [id_ "child"] "Child element"

withTestServer :: IO a -> IO a
withTestServer = bracket (forkIO $ run 8080 app) killThread . const

withFirefox :: IO a -> IO a
withFirefox =
    bracket
        ( startProcess
            . setStdout nullStream
            . setStderr nullStream
            . proc "firefox"
            $ [ "--marionette"
              , "--headless"
              ]
        )
        ( \process -> do
            runMarionette $ newSession >> quit
            checkExitCode process
        )
        . const

instance MonadError Error (MarionetteT IO) where
    throwError = liftIO . throwIO
    catchError action handler = action `catch` handler

run' :: (HasCallStack) => MarionetteT IO () -> IO ()
run' action = runMarionette $ do
    newSession
    navigate "http://localhost:8080/"
    action
    deleteSession

main :: IO ()
main = withFirefox . withTestServer . hspec $ do
    describe "Session" do
        it "can create and delete a session" . run' $ do
            deleteSession
            newSession

    describe "Navigation" do
        it "navigates to a URL" . run' $ do
            url <- getCurrentURL
            liftIO $ url `shouldBe` "http://localhost:8080/"

        it "can go back and forward" . run' $ do
            navigate "http://localhost:8080/target"
            back
            url <- getCurrentURL
            liftIO $ url `shouldBe` "http://localhost:8080/"
            forward
            url' <- getCurrentURL
            liftIO $ url' `shouldBe` "http://localhost:8080/target"

        it "can refresh" . run' $ do
            refresh
            url <- getCurrentURL
            liftIO $ url `shouldBe` "http://localhost:8080/"

        it "getTitle returns page title" . run' $ do
            title <- getTitle
            liftIO $ title `shouldBe` "Marionette Test"

        it "getPageSource returns HTML" . run' $ do
            src <- getPageSource
            liftIO $ src `shouldSatisfy` Text.isInfixOf "Hello, Marionette!"

        it "getCurrentURL returns current URL" . run' $ do
            url <- getCurrentURL
            liftIO $ url `shouldBe` "http://localhost:8080/"

    describe "Timeouts" do
        it "setTimeouts / getTimeouts roundtrip" . run' $ do
            let t =
                    Marionette.Timeouts
                        { script = Just 5000
                        , pageLoad = Just 10000
                        , implicit = Just 0
                        }
            setTimeouts t
            t' <- getTimeouts
            liftIO $ t' `shouldBe` t

        it "setTimeouts with partial update preserves other fields" . run' $ do
            let initial =
                    Marionette.Timeouts
                        { script = Just 3000
                        , pageLoad = Just 6000
                        , implicit = Just 500
                        }
            setTimeouts initial
            setTimeouts initial{script = Just 9000}
            t <- getTimeouts
            liftIO $ Marionette.pageLoad t `shouldBe` Just 6000
            liftIO $ Marionette.implicit t `shouldBe` Just 500

    describe "Window" do
        it "getWindowHandle returns a handle" . run' $ do
            h <- getWindowHandle
            liftIO $ h `shouldSatisfy` (not . null . show)

        it "getWindowHandles returns at least one handle" . run' $ do
            hs <- getWindowHandles
            liftIO $ length hs `shouldSatisfy` (>= 1)

        it "getWindowRect returns a rect" . run' $ do
            r <- getWindowRect
            liftIO $ Marionette.width r `shouldSatisfy` (> 0)
            liftIO $ Marionette.height r `shouldSatisfy` (> 0)

        it "setWindowRect / getWindowRect roundtrip" . run' $ do
            let r = Marionette.Rect{x = 0, y = 0, width = 800, height = 600}
            setWindowRect r
            r' <- getWindowRect
            liftIO $ Marionette.width r' `shouldBe` 800
            liftIO $ Marionette.height r' `shouldBe` 600

        it "maximizeWindow does not throw" $ run' maximizeWindow
        it "minimizeWindow does not throw" $ run' minimizeWindow
        it "fullscreenWindow does not throw" $ run' fullscreenWindow

        it "newTab opens a new window handle" . run' $ do
            before <- getWindowHandles
            _ <- newTab
            after <- getWindowHandles
            liftIO $ length after `shouldBe` length before + 1

        it "newWindow opens a new window handle" . run' $ do
            before <- getWindowHandles
            _ <- newWindow
            after <- getWindowHandles
            liftIO $ length after `shouldBe` length before + 1

        it "switchToWindow and closeWindow" . run' $ do
            original <- getWindowHandle
            result <- newTab
            switchToWindow (Marionette.newWindowHandle result)
            closeWindow
            switchToWindow original

    describe "Element finding" do
        it "findElement by id" . run' $ do
            el <- findElement (ById "heading")
            text <- getElementText el
            liftIO $ text `shouldBe` "Hello, Marionette!"

        it "findElement by class" . run' $ do
            el <- findElement (ByClass "content")
            text <- getElementText el
            liftIO $ text `shouldBe` "Test paragraph"

        it "findElement by tag" . run' $ do
            el <- findElement (ByTag "h1")
            text <- getElementText el
            liftIO $ text `shouldBe` "Hello, Marionette!"

        it "findElement by CSS selector" . run' $ do
            el <- findElement (ByCSS "#heading")
            text <- getElementText el
            liftIO $ text `shouldBe` "Hello, Marionette!"

        it "findElement by XPath" . run' $ do
            el <- findElement (ByXPath "//h1[@id='heading']")
            text <- getElementText el
            liftIO $ text `shouldBe` "Hello, Marionette!"

        it "findElements returns multiple elements" . run' $ do
            els :: [Element] <- findElements $ ByClass "content"
            liftIO $ length els `shouldBe` 2

        it "findElementFrom finds child within parent" . run' $ do
            parent <- findElement (ById "parent")
            child <- findElementFrom parent (ById "child")
            text <- getElementText child
            liftIO $ text `shouldBe` "Child element"

        it "findElementsFrom finds children within parent" . run' $ do
            parent <- findElement (ById "parent")
            children :: [Element] <- findElementsFrom parent $ ByTag "span"
            liftIO $ length children `shouldBe` 1

        it "findElement by link text" . run' $ do
            navigate "http://localhost:8080/link"
            el <- findElement (ByLinkText "Click me")
            text <- getElementText el
            liftIO $ text `shouldBe` "Click me"

        it "findElement by partial link text" . run' $ do
            navigate "http://localhost:8080/link"
            el <- findElement (ByPartialLinkText "Click here")
            text <- getElementText el
            liftIO $ text `shouldSatisfy` Text.isInfixOf "Click here"

    describe "Element properties" do
        it "getElementAttribute" . run' $ do
            el <- findElement (ById "para")
            attr <- getElementAttribute "class" el
            liftIO $ attr `shouldBe` Just "content"

        it "getElementProperty" . run' $ do
            el <- findElement (ById "para")
            prop <- getElementProperty el "id"
            liftIO $ prop `shouldBe` Just "para"

        it "getElementTagName" . run' $ do
            el <- findElement (ById "heading")
            tag <- getElementTagName el
            liftIO $ Text.toLower tag `shouldBe` "h1"

        it "getElementRect" . run' $ do
            el <- findElement (ById "heading")
            r <- getElementRect el
            liftIO $ Marionette.width r `shouldSatisfy` (> 0)

        it "getElementCSSValue" . run' $ do
            el <- findElement (ById "heading")
            val <- getElementCSSValue el "display"
            liftIO $ val `shouldSatisfy` (not . null . show)

        it "getComputedRole" . run' $ do
            el <- findElement (ById "heading")
            role <- getComputedRole el
            liftIO $ role `shouldSatisfy` (not . Text.null)

        it "getComputedLabel" . run' $ do
            navigate "http://localhost:8080/form"
            el <- findElement (ById "submit")
            label <- getComputedLabel el
            liftIO $ label `shouldSatisfy` (not . Text.null)

        it "isElementDisplayed" . run' $ do
            navigate "http://localhost:8080/"
            el <- findElement (ById "heading")
            displayed <- isElementDisplayed el
            liftIO $ displayed `shouldBe` True

        it "isElementEnabled" . run' $ do
            navigate "http://localhost:8080/form"
            el <- findElement (ById "text-input")
            enabled <- isElementEnabled el
            liftIO $ enabled `shouldBe` True

        it "isElementSelected for unchecked checkbox" . run' $ do
            navigate "http://localhost:8080/form"
            el <- findElement (ById "checkbox")
            selected <- isElementSelected el
            liftIO $ selected `shouldBe` False

        it "getActiveElement" . run' $ do
            _ <- getActiveElement
            pure ()

    describe "Element interaction" do
        it "elementClick navigates via link" . run' $ do
            navigate "http://localhost:8080/link"
            el <- findElement (ById "link")
            elementClick el
            url <- getCurrentURL
            liftIO $ url `shouldBe` "http://localhost:8080/target"

        it "elementSendKeys fills input" . run' $ do
            navigate "http://localhost:8080/form"
            el <- findElement (ById "text-input")
            elementSendKeys el "hello"
            val <- getElementProperty el "value"
            liftIO $ val `shouldBe` Just "hello"

        it "elementClear clears input" . run' $ do
            navigate "http://localhost:8080/form"
            el <- findElement (ById "text-input")
            elementSendKeys el "hello"
            elementClear el
            val <- getElementProperty el "value"
            liftIO $ val `shouldBe` Just ""

    describe "Script execution" do
        it "executeScript returns a value" . run' $ do
            result <- executeScript "return 1 + 1" []
            liftIO $ result `shouldBe` (2 :: Int)

        it "executeScript can access elements" . run' $ do
            navigate "http://localhost:8080/"
            result <-
                executeScript
                    "return document.getElementById(arguments[0]).textContent"
                    [toJSON @Text "heading"]
            liftIO $ result `shouldBe` ("Hello, Marionette!" :: Text)

        it "executeAsyncScript returns a value" . run' $ do
            result <-
                executeAsyncScript @_ @_ @Int
                    "setTimeout(() => arguments[arguments.length-1](42), 100)"
                    []
            liftIO $ result `shouldBe` Just 42

    describe "Cookies" do
        it "addCookie / getCookies roundtrip" . run' $ do
            deleteAllCookies
            let cookie =
                    Marionette.Cookie
                        { name = "test"
                        , value = "value"
                        , path = Just "/"
                        , domain = Nothing
                        , secure = Just False
                        , httpOnly = Just False
                        , expiry = Nothing
                        }
            addCookie cookie
            cookies <- getCookies
            liftIO $
                any (\c -> name c == "test") cookies
                    `shouldBe` True

        it "deleteCookie removes a cookie" . run' $ do
            deleteAllCookies
            let cookie =
                    Marionette.Cookie
                        { name = "todelete"
                        , value = "x"
                        , path = Just "/"
                        , domain = Nothing
                        , secure = Just False
                        , httpOnly = Just False
                        , expiry = Nothing
                        }
            addCookie cookie
            deleteCookie "todelete"
            cookies <- getCookies
            liftIO $
                any (\c -> name c == "todelete") cookies
                    `shouldBe` False

        it "deleteAllCookies removes all cookies" . run' $ do
            deleteAllCookies
            cookies <- getCookies
            liftIO $ cookies `shouldBe` []

    describe "Alerts" do
        it "acceptAlert dismisses an alert" . run' $ do
            navigate "http://localhost:8080/alert"
            el <- findElement (ById "alert-btn")
            elementClick el
            text <- getAlertText
            liftIO $ text `shouldBe` "Hello!"
            acceptAlert

        it "dismissAlert dismisses a confirm dialog" . run' $ do
            navigate "http://localhost:8080/alert"
            el <- findElement (ById "confirm-btn")
            elementClick el
            dismissAlert

        it "sendAlertText fills a prompt" . run' $ do
            navigate "http://localhost:8080/alert"
            el <- findElement (ById "prompt-btn")
            elementClick el
            sendAlertText "my input"
            acceptAlert

    describe "Frames" do
        it "switchToParentFrame does not throw" . run' $ do
            switchToParentFrame

    describe "Context" do
        it "getContext returns a context" . run' $ do
            ctx <- getContext
            liftIO $
                ctx
                    `shouldSatisfy` \c -> c == Marionette.ContentContext || c == Marionette.ChromeContext

        it "setContext / getContext roundtrip" . run' $ do
            setContext Marionette.ContentContext
            ctx <- getContext
            liftIO $ ctx `shouldBe` Marionette.ContentContext

    describe "Shadow DOM" do
        it "getShadowRoot / findElementFromShadowRoot" . run' $ do
            navigate "http://localhost:8080/shadow"
            host <- findElement (ById "host")
            shadow <- getShadowRoot host
            el <- findElementFromShadowRoot shadow (ByCSS "#shadow-p")
            text <- getElementText el
            liftIO $ text `shouldBe` "Shadow content"

        it "findElementsFromShadowRoot" . run' $ do
            navigate "http://localhost:8080/shadow"
            host <- findElement (ById "host")
            shadow <- getShadowRoot host
            els :: [Element] <- findElementsFromShadowRoot shadow $ ByCSS "p"
            liftIO $ length els `shouldBe` 1

    describe "Screenshots" do
        it "takeScreenshot returns non-empty bytes" . run' $ do
            bytes <- takeScreenshot
            liftIO $ ByteString.length bytes `shouldNotBe` 0

    describe "WebAuthn" do
        it "addVirtualAuthenticator / getCredentials roundtrip" . run' $ do
            let opts =
                    VirtualAuthenticator
                        { protocol = "ctap2"
                        , transport = "internal"
                        , hasResidentKey = True
                        , hasUserVerification = True
                        , isUserConsenting = True
                        , isUserVerified = True
                        , extensions = Nothing
                        , uvm = Nothing
                        }
            aid <- addVirtualAuthenticator opts
            creds <- getCredentials aid
            liftIO $ creds `shouldBe` []
            removeVirtualAuthenticator aid

        it "setUserVerified does not throw" . run' $ do
            let opts =
                    VirtualAuthenticator
                        { protocol = "ctap2"
                        , transport = "internal"
                        , hasResidentKey = True
                        , hasUserVerification = True
                        , isUserConsenting = True
                        , isUserVerified = True
                        , extensions = Nothing
                        , uvm = Nothing
                        }
            aid <- addVirtualAuthenticator opts
            setUserVerified aid False
            setUserVerified aid True
            removeVirtualAuthenticator aid

        it "removeAllCredentials does not throw" . run' $ do
            let opts =
                    VirtualAuthenticator
                        { protocol = "ctap2"
                        , transport = "internal"
                        , hasResidentKey = True
                        , hasUserVerification = True
                        , isUserConsenting = True
                        , isUserVerified = True
                        , extensions = Nothing
                        , uvm = Nothing
                        }
            aid <- addVirtualAuthenticator opts
            removeAllCredentials aid
            removeVirtualAuthenticator aid
