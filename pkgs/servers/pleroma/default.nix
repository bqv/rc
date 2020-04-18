{ stdenv, lib, fetchurl, fetchzip, writeShellScriptBin, makeWrapper,
  meson, ninja, pkgconfig, elixir, rebar, rebar3, nodejs, curl,
  glibcLocales, glib, git, buildLocale ? "en_US.UTF-8",
  domain ? "pleroma.local",
  title ? "Pleroma/local",
  adminEmail ? "pleroma@mail.local",
  senderEmail ? "pleroma@mail.local",
  searchEngineAllowIndex ? true,
  dbHost ? "localhost",
  dbName ? "pleroma",
  dbUser ? "pleroma",
  dbPass ? "hunter2",
  dbUseRumIndices ? true,
  listenPort ? 4000,
  listenIP ? "127.0.0.1",
  uploadsDir ? "/var/lib/pleroma/upload/",
  publicDir ? "/var/lib/pleroma/public/" }:

with stdenv.lib;

let
  gzseek = stdenv.mkDerivation {
    name = "gzseek";

    src = fetchzip {
      url = "https://github.com/serokell/gzseek/archive/742ea67e84c8dfffcca3b239320cc9f321487d75.tar.gz";
      sha256 = "0pi871r60kh5xjlkz8paxsasqqjbgdddk8g4hmp4cp32cbx69xlw";
    };

    nativeBuildInputs = [ meson ninja pkgconfig ];
    buildInputs = [ glib ];
  };

  hex = fetchurl {
    url = "https://repo.hex.pm/installs/1.6.0/hex-0.18.1.ez";
    sha512 = "9c806664a3341930df4528be38b0da216a31994830c4437246555fe3027c047e073415bcb1b6557a28549e12b0c986142f5e3485825a66033f67302e87520119";
  };

  fetchHex = { pname, version, sha256 }: stdenv.mkDerivation {
    name = "hex-${pname}-${version}";

    src = fetchurl {
      url = "https://repo.hex.pm/tarballs/${pname}-${version}.tar";
      inherit sha256;

      postFetch = ''
        tar -xf $downloadedFile
        cat VERSION metadata.config contents.tar.gz > $out
      '';
    };

    buildCommand = ''
      mkdir $out && cd $out
      ${gzseek}/bin/gzseek $src | tar -zxf -
      echo ${pname},${version},${sha256},hexpm > .hex
    '';
  };

  closure = {
    hackney = fetchHex {
      pname = "hackney";
      version = "1.15.2";
      sha256 = "07e33c794f8f8964ee86cebec1a8ed88db5070e52e904b8f12209773c1036085";
    };
    gettext = fetchHex {
      pname = "gettext";
      version = "0.17.0";
      sha256 = "abe21542c831887a2b16f4c94556db9c421ab301aee417b7c4fbde7fbdbe01ec";
    };
    combine = fetchHex {
      pname = "combine";
      version = "0.10.0";
      sha256 = "eff8224eeb56498a2af13011d142c5e7997a80c8f5b97c499f84c841032e429f";
    };
    db_connection = fetchHex {
      pname = "db_connection";
      version = "2.1.1";
      sha256 = "a51e8a2ee54ef2ae6ec41a668c85787ed40cb8944928c191280fe34c15b76ae5";
    };
    accept = fetchHex {
      pname = "accept";
      version = "0.3.5";
      sha256 = "b33b127abca7cc948bbe6caa4c263369abf1347cfa9d8e699c6d214660f10cd1";
    };
    esshd = fetchHex {
      pname = "esshd";
      version = "0.1.0";
      sha256 = "6f93a2062adb43637edad0ea7357db2702a4b80dd9683482fe00f5134e97f4c1";
    };
    meck = fetchHex {
      pname = "meck";
      version = "0.8.13";
      sha256 = "ffedb39f99b0b99703b8601c6f17c7f76313ee12de6b646e671e3188401f7866";
    };
    timex = fetchHex {
      pname = "timex";
      version = "3.6.1";
      sha256 = "efdf56d0e67a6b956cc57774353b0329c8ab7726766a11547e529357ffdc1d56";
    };
    custom_base = fetchHex {
      pname = "custom_base";
      version = "0.2.1";
      sha256 = "4a832a42ea0552299d81652aa0b1f775d462175293e99dfbe4d7dbaab785a706";
    };
    ecto = fetchHex {
      pname = "ecto";
      version = "3.1.4";
      sha256 = "69d852da7a9f04ede725855a35ede48d158ca11a404fe94f8b2fb3b2162cd3c9";
    };
    certifi = fetchHex {
      pname = "certifi";
      version = "2.5.1";
      sha256 = "867ce347f7c7d78563450a18a6a28a8090331e77fa02380b4a21962a65d36ee5";
    };
    eternal = fetchHex {
      pname = "eternal";
      version = "1.2.0";
      sha256 = "e2a6b6ce3b8c248f7dc31451aefca57e3bdf0e48d73ae5043229380a67614c41";
    };
    plug_cowboy = fetchHex {
      pname = "plug_cowboy";
      version = "2.1.0";
      sha256 = "b75768153c3a8a9e8039d4b25bb9b14efbc58e9c4a6e6a270abff1cd30cbe320";
    };
    ex_aws_s3 = fetchHex {
      pname = "ex_aws_s3";
      version = "2.0.1";
      sha256 = "9e09366e77f25d3d88c5393824e613344631be8db0d1839faca49686e99b6704";
    };
    mimerl = fetchHex {
      pname = "mimerl";
      version = "1.2.0";
      sha256 = "67e2d3f571088d5cfd3e550c383094b47159f3eee8ffa08e64106cdf5e981be3";
    };
    comeonin = fetchHex {
      pname = "comeonin";
      version = "4.1.2";
      sha256 = "3eb5620fd8e35508991664b4c2b04dd41e52f1620b36957be837c1d7784b7592";
    };
    gen_smtp = fetchHex {
      pname = "gen_smtp";
      version = "0.14.0";
      sha256 = "39846a03522456077c6429b4badfd1d55e5e7d0fdfb65e935b7c5e38549d9202";
    };
    mogrify = fetchHex {
      pname = "mogrify";
      version = "0.6.1";
      sha256 = "de1b527514f2d95a7bbe9642eb556061afb337e220cf97adbf3a4e6438ed70af";
    };
    ranch = fetchHex {
      pname = "ranch";
      version = "1.7.1";
      sha256 = "6b1fab51b49196860b733a49c07604465a47bdb78aa10c1c16a3d199f7f8c881";
    };
    plug_static_index_html = fetchHex {
      pname = "plug_static_index_html";
      version = "1.0.0";
      sha256 = "840123d4d3975585133485ea86af73cb2600afd7f2a976f9f5fd8b3808e636a0";
    };
    captcha = fetchGit {
      url = "https://git.pleroma.social/pleroma/elixir-libraries/elixir-captcha.git";
      rev = "e0f16822d578866e186a0974d65ad58cddc1e2ab";
    };
    http_signatures = fetchGit {
      url = "https://git.pleroma.social/pleroma/http_signatures.git";
      rev = "293d77bb6f4a67ac8bde1428735c3b42f22cbb30";
    };
    decimal = fetchHex {
      pname = "decimal";
      version = "1.8.0";
      sha256 = "ca462e0d885f09a1c5a342dbd7c1dcf27ea63548c65a65e67334f4b61803822e";
    };
    unicode_util_compat = fetchHex {
      pname = "unicode_util_compat";
      version = "0.4.1";
      sha256 = "d869e4c68901dd9531385bb0c8c40444ebf624e60b6962d95952775cac5e90cd";
    };
    mime = fetchHex {
      pname = "mime";
      version = "1.3.1";
      sha256 = "30ce04ab3175b6ad0bdce0035cba77bba68b813d523d1aac73d9781b4d193cf8";
    };
    ex_const = fetchHex {
      pname = "ex_const";
      version = "0.2.4";
      sha256 = "d06e540c9d834865b012a17407761455efa71d0ce91e5831e86881b9c9d82448";
    };
    ex2ms = fetchHex {
      pname = "ex2ms";
      version = "1.5.0";
      sha256 = "19e27f9212be9a96093fed8cdfbef0a2b56c21237196d26760f11dfcfae58e97";
    };
    nimble_parsec = fetchHex {
      pname = "nimble_parsec";
      version = "0.5.1";
      sha256 = "c90796ecee0289dbb5ad16d3ad06f957b0cd1199769641c961cfe0b97db190e0";
    };
    plug_crypto = fetchHex {
      pname = "plug_crypto";
      version = "1.0.0";
      sha256 = "18e49317d3fa343f24620ed22795ec29d4a5e602d52d1513ccea0b07d8ea7d4d";
    };
    cachex = fetchHex {
      pname = "cachex";
      version = "3.0.3";
      sha256 = "4e2d3e05814a5738f5ff3903151d5c25636d72a3527251b753f501ad9c657967";
    };
    prometheus_phoenix = fetchHex {
      pname = "prometheus_phoenix";
      version = "1.3.0";
      sha256 = "c4b527e0b3a9ef1af26bdcfbfad3998f37795b9185d475ca610fe4388fdd3bb5";
    };
    poolboy = fetchHex {
      pname = "poolboy";
      version = "1.5.2";
      sha256 = "392b007a1693a64540cead79830443abf5762f5d30cf50bc95cb2c1aaafa006b";
    };
    trailing_format_plug = fetchHex {
      pname = "trailing_format_plug";
      version = "0.0.7";
      sha256 = "64b877f912cf7273bed03379936df39894149e35137ac9509117e59866e10e45";
    };
    ex_rated = fetchHex {
      pname = "ex_rated";
      version = "1.3.3";
      sha256 = "30ecbdabe91f7eaa9d37fa4e81c85ba420f371babeb9d1910adbcd79ec798d27";
    };
    ex_doc = fetchHex {
      pname = "ex_doc";
      version = "0.21.2";
      sha256 = "caca5bc28ed7b3bdc0b662f8afe2bee1eedb5c3cf7b322feeeb7c6ebbde089d6";
    };
    postgrex = fetchHex {
      pname = "postgrex";
      version = "0.14.3";
      sha256 = "5754dee2fdf6e9e508cbf49ab138df964278700b764177e8f3871e658b345a1e";
    };
    ecto_sql = fetchHex {
      pname = "ecto_sql";
      version = "3.1.3";
      sha256 = "2c536139190492d9de33c5fefac7323c5eaaa82e1b9bf93482a14649042f7cd9";
    };
    recon = fetchGit {
      url = "https://github.com/ferd/recon.git";
      rev = "75d70c7c08926d2f24f1ee6de14ee50fe8a52763";
    };
    html_sanitize_ex = fetchHex {
      pname = "html_sanitize_ex";
      version = "1.3.0";
      sha256 = "f005ad692b717691203f940c686208aa3d8ffd9dd4bb3699240096a51fa9564e";
    };
    deep_merge = fetchHex {
      pname = "deep_merge";
      version = "1.0.0";
      sha256 = "b4aa1a0d1acac393bdf38b2291af38cb1d4a52806cf7a4906f718e1feb5ee961";
    };
    connection = fetchHex {
      pname = "connection";
      version = "1.0.4";
      sha256 = "a1cae72211f0eef17705aaededacac3eb30e6625b04a6117c1b2db6ace7d5976";
    };
    flake_id = fetchHex {
      pname = "flake_id";
      version = "0.1.0";
      sha256 = "7716b086d2e405d09b647121a166498a0d93d1a623bead243e1f74216079ccb3";
    };
    idna = fetchHex {
      pname = "idna";
      version = "6.0.0";
      sha256 = "689c46cbcdf3524c44d5f3dde8001f364cd7608a99556d8fbd8239a5798d4c10";
    };
    makeup = fetchHex {
      pname = "makeup";
      version = "1.0.0";
      sha256 = "671df94cf5a594b739ce03b0d0316aa64312cee2574b6a44becb83cd90fb05dc";
    };
    auto_linker = fetchGit {
      url = "https://git.pleroma.social/pleroma/auto_linker.git";
      rev = "95e8188490e97505c56636c1379ffdf036c1fdde";
    };
    joken = fetchHex {
      pname = "joken";
      version = "2.0.1";
      sha256 = "ec9ab31bf660f343380da033b3316855197c8d4c6ef597fa3fcb451b326beb14";
    };
    tzdata = fetchHex {
      pname = "tzdata";
      version = "0.5.21";
      sha256 = "8cbf3607fcce69636c672d5be2bbb08687fe26639a62bdcc283d267277db7cf0";
    };
    pbkdf2_elixir = fetchHex {
      pname = "pbkdf2_elixir";
      version = "0.12.3";
      sha256 = "6706a148809a29c306062862c803406e88f048277f6e85b68faf73291e820b84";
    };
    unsafe = fetchHex {
      pname = "unsafe";
      version = "1.0.1";
      sha256 = "a27e1874f72ee49312e0a9ec2e0b27924214a05e3ddac90e91727bc76f8613d8";
    };
    parse_trans = fetchHex {
      pname = "parse_trans";
      version = "3.3.0";
      sha256 = "09765507a3c7590a784615cfd421d101aec25098d50b89d7aa1d66646bc571c1";
    };
    websocket_client = fetchGit {
      url = "https://github.com/jeremyong/websocket_client.git";
      rev = "9a6f65d05ebf2725d62fb19262b21f1805a59fbf";
    };
    jason = fetchHex {
      pname = "jason";
      version = "1.1.2";
      sha256 = "b03dedea67a99223a2eaf9f1264ce37154564de899fd3d8b9a21b1a6fd64afe7";
    };
    phoenix = fetchHex {
      pname = "phoenix";
      version = "1.4.9";
      sha256 = "746d098e10741c334d88143d3c94cab1756435f94387a63441792e66ec0ee974";
    };
    tesla = fetchHex {
      pname = "tesla";
      version = "1.3.0";
      sha256 = "f35d72f029e608f9cdc6f6d6fcc7c66cf6d6512a70cfef9206b21b8bd0203a30";
    };
    web_push_encryption = fetchHex {
      pname = "web_push_encryption";
      version = "0.2.1";
      sha256 = "d42cecf73420d9dc0053ba3299cc8c8d6ff2be2487d67ca2a57265868e4d9a98";
    };
    cowboy = fetchHex {
      pname = "cowboy";
      version = "2.6.3";
      sha256 = "99aa50e94e685557cad82e704457336a453d4abcb77839ad22dbe71f311fcc06";
    };
    prometheus = fetchHex {
      pname = "prometheus";
      version = "4.4.1";
      sha256 = "1e96073b3ed7788053768fea779cbc896ddc3bdd9ba60687f2ad50b252ac87d6";
    };
    quack = fetchHex {
      pname = "quack";
      version = "0.1.1";
      sha256 = "cca7b4da1a233757fdb44b3334fce80c94785b3ad5a602053b7a002b5a8967bf";
    };
    prometheus_ecto = fetchHex {
      pname = "prometheus_ecto";
      version = "1.4.1";
      sha256 = "6c768ea9654de871e5b32fab2eac348467b3021604ebebbcbd8bcbe806a65ed5";
    };
    cowlib = fetchHex {
      pname = "cowlib";
      version = "2.7.3";
      sha256 = "a7ffcd0917e6d50b4d5fb28e9e2085a0ceb3c97dea310505f7460ff5ed764ce9";
    };
    bunt = fetchHex {
      pname = "bunt";
      version = "0.2.0";
      sha256 = "951c6e801e8b1d2cbe58ebbd3e616a869061ddadcc4863d0a2182541acae9a38";
    };
    mock = fetchHex {
      pname = "mock";
      version = "0.3.3";
      sha256 = "42a433794b1291a9cf1525c6d26b38e039e0d3a360732b5e467bfc77ef26c914";
    };
    html_entities = fetchHex {
      pname = "html_entities";
      version = "0.5.0";
      sha256 = "40f5c5b9cbe23073b48a4e69c67b6c11974f623a76165e2b92d098c0e88ccb1d";
    };
    telemetry = fetchHex {
      pname = "telemetry";
      version = "0.4.0";
      sha256 = "8339bee3fa8b91cb84d14c2935f8ecf399ccd87301ad6da6b71c09553834b2ab";
    };
    excoveralls = fetchHex {
      pname = "excoveralls";
      version = "0.11.1";
      sha256 = "dd677fbdd49114fdbdbf445540ec735808250d56b011077798316505064edb2c";
    };
    mox = fetchHex {
      pname = "mox";
      version = "0.5.1";
      sha256 = "f86bb36026aac1e6f924a4b6d024b05e9adbed5c63e8daa069bd66fb3292165b";
    };
    floki = fetchHex {
      pname = "floki";
      version = "0.20.4";
      sha256 = "be42ac911fece24b4c72f3b5846774b6e61b83fe685c2fc9d62093277fb3bc86";
    };
    phoenix_html = fetchHex {
      pname = "phoenix_html";
      version = "2.13.1";
      sha256 = "fa8f034b5328e2dfa0e4131b5569379003f34bc1fafdaa84985b0b9d2f12e68b";
    };
    earmark = fetchHex {
      pname = "earmark";
      version = "1.3.6";
      sha256 = "ce1d0675e10a5bb46b007549362bd3f5f08908843957687d8484fe7f37466b19";
    };
    prometheus_plugs = fetchHex {
      pname = "prometheus_plugs";
      version = "1.1.5";
      sha256 = "25933d48f8af3a5941dd7b621c889749894d8a1082a6ff7c67cc99dec26377c5";
    };
    jose = fetchHex {
      pname = "jose";
      version = "1.9.0";
      sha256 = "4167c5f6d06ffaebffd15cdb8da61a108445ef5e85ab8f5a7ad926fdf3ada154";
    };
    makeup_elixir = fetchHex {
      pname = "makeup_elixir";
      version = "0.14.0";
      sha256 = "cf8b7c66ad1cff4c14679698d532f0b5d45a3968ffbcbfd590339cb57742f1ae";
    };
    mochiweb = fetchHex {
      pname = "mochiweb";
      version = "2.18.0";
      sha256 = "eb55f1db3e6e960fac4e6db4e2db9ec3602cc9f30b86cd1481d56545c3145d2e";
    };
    ex_machina = fetchHex {
      pname = "ex_machina";
      version = "2.3.0";
      sha256 = "92a5ad0a8b10ea6314b876a99c8c9e3f25f4dde71a2a835845b136b9adaf199a";
    };
    base64url = fetchHex {
      pname = "base64url";
      version = "0.0.1";
      sha256 = "36a90125f5948e3afd7be97662a1504b934dd5dac78451ca6e9abf85a10286be";
    };
    crontab = fetchHex {
      pname = "crontab";
      version = "1.1.7";
      sha256 = "b9219f0bdc8678b94143655a8f229716c5810c0636a4489f98c0956137e53985";
    };
    pleroma_job_queue = fetchHex {
      pname = "pleroma_job_queue";
      version = "0.3.0";
      sha256 = "b84538d621f0c3d6fcc1cff9d5648d3faaf873b8b21b94e6503428a07a48ec47";
    };
    phoenix_ecto = fetchHex {
      pname = "phoenix_ecto";
      version = "4.0.0";
      sha256 = "c43117a136e7399ea04ecaac73f8f23ee0ffe3e07acfcb8062fe5f4c9f0f6531";
    };
    poison = fetchHex {
      pname = "poison";
      version = "3.1.0";
      sha256 = "d9eb636610e096f86f25d9a46f35a9facac35609a7591b3be3326e99a0484665";
    };
    ssl_verify_fun = fetchHex {
      pname = "ssl_verify_fun";
      version = "1.1.5";
      sha256 = "6eaf7ad16cb568bb01753dbbd7a95ff8b91c7979482b95f38443fe2c8852a79b";
    };
    httpoison = fetchHex {
      pname = "httpoison";
      version = "1.2.0";
      sha256 = "2702ed3da5fd7a8130fc34b11965c8cfa21ade2f232c00b42d96d4967c39a3a3";
    };
    metrics = fetchHex {
      pname = "metrics";
      version = "1.0.1";
      sha256 = "25f094dea2cda98213cecc3aeff09e940299d950904393b2a29d191c346a8486";
    };
    plug = fetchHex {
      pname = "plug";
      version = "1.8.2";
      sha256 = "0bcce1daa420f189a6491f3940cc77ea7fb1919761175c9c3b59800d897440fc";
    };
    crypt = fetchGit {
      url = "https://github.com/msantos/crypt";
      rev = "1f2b58927ab57e72910191a7ebaeff984382a1d3";
    };
    cors_plug = fetchHex {
      pname = "cors_plug";
      version = "1.5.2";
      sha256 = "72df63c87e4f94112f458ce9d25800900cc88608c1078f0e4faddf20933eda6e";
    };
    benchee = fetchHex {
      pname = "benchee";
      version = "1.0.1";
      sha256 = "66b211f9bfd84bd97e6d1beaddf8fc2312aaabe192f776e8931cb0c16f53a521";
    };
    ex_syslogger = fetchGit {
      url = "https://github.com/slashmili/ex_syslogger.git";
      rev = "f3963399047af17e038897c69e20d552e6899e1d";
    };
    credo = fetchHex {
      pname = "credo";
      version = "0.9.3";
      sha256 = "76fa3e9e497ab282e0cf64b98a624aa11da702854c52c82db1bf24e54ab7c97a";
    };
    phoenix_swoosh = fetchHex {
      pname = "phoenix_swoosh";
      version = "0.2.0";
      sha256 = "a7e0b32077cd6d2323ae15198839b05d9caddfa20663fd85787479e81f89520e";
    };
    phoenix_pubsub = fetchHex {
      pname = "phoenix_pubsub";
      version = "1.1.2";
      sha256 = "496c303bdf1b2e98a9d26e89af5bba3ab487ba3a3735f74bf1f4064d2a845a3e";
    };
    base62 = fetchHex {
      pname = "base62";
      version = "1.2.1";
      sha256 = "4866763e08555a7b3917064e9eef9194c41667276c51b59de2bc42c6ea65f806";
    };
    prometheus_ex = fetchHex {
      pname = "prometheus_ex";
      version = "3.0.5";
      sha256 = "fa58cfd983487fc5ead331e9a3e0aa622c67232b3ec71710ced122c4c453a02f";
    };
    bbcode = fetchHex {
      pname = "bbcode";
      version = "0.1.1";
      sha256 = "0023e2c7814119b2e620b7add67182e3f6019f92bfec9a22da7e99821aceba70";
    };
    syslog = fetchGit {
      url = "https://github.com/Vagabond/erlang-syslog.git";
      rev = "4a6c6f2c996483e86c1320e9553f91d337bcb6aa";
    };
    calendar = fetchHex {
      pname = "calendar";
      version = "0.17.6";
      sha256 = "ec291cb2e4ba499c2e8c0ef5f4ace974e2f9d02ae9e807e711a9b0c7850b9aee";
    };
    ueberauth = fetchHex {
      pname = "ueberauth";
      version = "0.6.1";
      sha256 = "9e90d3337dddf38b1ca2753aca9b1e53d8a52b890191cdc55240247c89230412";
    };
    ex_aws = fetchHex {
      pname = "ex_aws";
      version = "2.1.0";
      sha256 = "b92651527d6c09c479f9013caa9c7331f19cba38a650590d82ebf2c6c16a1d8a";
    };
    swoosh = fetchHex {
      pname = "swoosh";
      version = "0.23.2";
      sha256 = "7dda95ff0bf54a2298328d6899c74dae1223777b43563ccebebb4b5d2b61df38";
    };
    sweet_xml = fetchHex {
      pname = "sweet_xml";
      version = "0.6.6";
      sha256 = "fc3e91ec5dd7c787b6195757fbcf0abc670cee1e4172687b45183032221b66b8";
    };

  };

  # Deep link deps so mix can write build files in the dep dirs, then fool mix
  # to think any git deps are checked out.
  linkDependency = name: src: ''
    cp -rs --no-preserve=mode ${src} deps/${name}
    mkdir deps/${name}/.git
    echo ${if lib.hasAttr "rev" src
           then src.rev
           else ""} > deps/${name}/.git/HEAD
  '';

  configArgs = [
    "--force"
    "--output config/prod.secret.exs"
    "--output-psql config/setup_db.psql"
    "--domain ${lib.escapeShellArg domain}"
    "--instance-name ${lib.escapeShellArg title}"
    "--admin-email ${lib.escapeShellArg adminEmail}"
    "--notify-email ${lib.escapeShellArg senderEmail}"
    "--dbhost ${lib.escapeShellArg dbHost}"
    "--dbname ${lib.escapeShellArg dbName}"
    "--dbuser ${lib.escapeShellArg dbUser}"
    "--dbpass ${lib.escapeShellArg dbPass}"
    "--rum ${if dbUseRumIndices then "Y" else "N"}"
    "--indexable ${if searchEngineAllowIndex then "Y" else "N"}"
    "--uploads-dir ${lib.escapeShellArg uploadsDir}"
    "--static-dir ${lib.escapeShellArg "instances/static"}"
    "--listen-ip ${lib.escapeShellArg "uploads"}"
    "--listen-port ${lib.escapeShellArg listenPort}"
  ];

  pleroma_be = let
    version = "1.1.8";
    fakeGit = (writeShellScriptBin "git" ''
      [[ "$@" =~ "describe" ]] && [[ "$@" =~ "abbrev=0" ]] && echo v${version} && exit 0
      [[ "$@" =~ "describe" ]] && [[ "$@" =~ "abbrev=8" ]] && echo v${version} && exit 0
      [[ "$@" =~ "rev-parse" ]] && [[ "$@" =~ "short" ]] && echo 000000000 && exit 0
      [[ "$@" =~ "rev-parse" ]] && [[ "$@" =~ "ref" ]] && echo nix && exit 0
      exec ${git}/bin/git $@
    '');
  in stdenv.mkDerivation {
    name = "pleroma-be";
    inherit version;
  
    src = fetchGit {
      url = "https://git.pleroma.social/pleroma/pleroma";
      ref = "stable";
      rev = "1629fa2412b877f69c5cf0df09782227827b272b";
    };
  
    buildInputs = [ glibcLocales elixir rebar rebar3 fakeGit makeWrapper ];
  
    configurePhase = ''
      runHook preConfigure
  
      mkdir deps
      ${concatStringsSep "\n" (mapAttrsToList linkDependency closure)}
  
      runHook postConfigure
    '';
  
    HOME = ".";
  
    MIX_ENV = "prod";
    MIX_REBAR = "${rebar}/bin/rebar";
    MIX_REBAR3 = "${rebar3}/bin/rebar3";
  
    LANG = "${buildLocale}";
    LC_ALL = "${buildLocale}";
  
    buildPhase = let genArgs = concatStringsSep " " configArgs; in ''
      runHook preBuild
  
      touch config/prod.secret.exs
  
      mix archive.install --force ${hex}
      mix deps.compile --force --include-children
      mix compile --no-deps-check
      mix do deps.loadpaths --no-deps-check, pleroma.instance gen ${genArgs}

      sed -i '/static_dir:/s#".*"#"${publicDir}"#' config/prod.secret.exs
      sed -i '/uploads:/s#".*"#"${uploadsDir}"#' config/prod.secret.exs
  
      runHook postBuild
    '';
  
    installPhase = ''
      runHook preInstall
  
      mkdir -p $out/bin
      mv * .mix $out
      
      makeWrapper ${elixir}/bin/mix $out/bin/pleroma-mix \
        --run "cd $out" \
        --prefix PATH : ${lib.makeBinPath [ fakeGit ]} \
        --add-flags "do deps.loadpaths --no-deps-check," \
        --set HOME $out \
        --set-default MIX_ENV prod \
        --set MIX_REBAR ${rebar}/bin/rebar \
        --set MIX_REBAR3 ${rebar3}/bin/rebar3
  
      runHook postInstall
    '';
  };

  pleromaNodeDeps = let
    nodePackages = import ./pleroma-fe { inherit nodejs; };
    in nodePackages.shell.nodeDependencies;

  pleroma_fe = let
    version = "1.0.0";
    fakeGit = (writeShellScriptBin "git" ''
      [[ "$@" =~ "describe" ]] && [[ "$@" =~ "abbrev=0" ]] && echo v${version} && exit 0
      [[ "$@" =~ "describe" ]] && [[ "$@" =~ "abbrev=8" ]] && echo v${version} && exit 0
      [[ "$@" =~ "rev-parse" ]] && [[ "$@" =~ "short" ]] && echo 000000000 && exit 0
      [[ "$@" =~ "rev-parse" ]] && [[ "$@" =~ "ref" ]] && echo nix && exit 0
      exec ${git}/bin/git $@
    '');
    fontelloSrc = stdenv.mkDerivation {
      name = "pleroma-fontello-src";

      src = fetchurl {
        url = "./fontello.zip";
        sha1 = "0000000000000000000000000000000000000000";
      };
    };
  in stdenv.mkDerivation {
    name = "pleroma-fe";
    inherit version;
  
    src = fetchGit {
      url = "https://git.pleroma.social/pleroma/pleroma-fe";
      ref = "master";
      rev = "3ab128e73924ce34d190ff609cb9b081cdffe402";
    };

    buildInputs = [ nodejs curl fontelloSrc fakeGit ];
 
    buildPhase = ''
      ln -s ${pleromaNodeDeps}/lib/node_modules

     #curl -sfS -F config=@static/fontello.json -o .fontello http://fontello.com
     #curl -sfS -o .fontello.zip http://fontello.com/$(cat .fontello)/get
     #unzip .fontello.zip -d .fontello.src
     #mv .fontello.src/*
    '';
 
    installPhase = ''
      cp -r . $out
      mkdir -p $out/bin
      ln -s ${fakeGit}/bin/git $out/bin/
    '';

    meta.broken = true;
  };

  mastoNodeDeps = let
    nodePackages = import ./masto-fe { inherit nodejs; };
    in nodePackages.shell.nodeDependencies;

  masto_fe = let
    version = "2.9.2";
    fakeGit = (writeShellScriptBin "git" ''
      [[ "$@" =~ "describe" ]] && [[ "$@" =~ "abbrev=0" ]] && echo v${version} && exit 0
      [[ "$@" =~ "describe" ]] && [[ "$@" =~ "abbrev=8" ]] && echo v${version} && exit 0
      [[ "$@" =~ "rev-parse" ]] && [[ "$@" =~ "short" ]] && echo 000000000 && exit 0
      [[ "$@" =~ "rev-parse" ]] && [[ "$@" =~ "ref" ]] && echo nix && exit 0
      exec ${git}/bin/git $@
    '');
  in stdenv.mkDerivation {
    name = "masto-fe";
    inherit version;
  
    src = fetchGit {
      url = "https://git.pleroma.social/pleroma/mastofe";
      ref = "rebase/glitch-soc";
      rev = "046df01415d52989c3e86b135c4deac972fe9134";
    };
 
    patches = [ ./masto-fe/postcss.patch ];

    buildInputs = [ nodejs fakeGit ];
 
    buildPhase = ''
      ln -s ${mastoNodeDeps}/lib/node_modules
      env HOME=. npm run build
    '';
 
    installPhase = ''
      cp -r . $out
    '';

    meta.broken = true;
  };
in {
  inherit pleroma_be;
  inherit pleroma_fe;
  inherit masto_fe;
}
