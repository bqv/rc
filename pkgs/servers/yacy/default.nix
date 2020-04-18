{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  pname         = "yacy";
  name          = "${pname}-${version}";
  version       = "1.922";
  revisiondate  = "20191013";
  revisionbuild = "9964";

  src = fetchurl {
    url = "http://latest.yacy.net/${pname}_v${version}_${revisiondate}_${revisionbuild}.tar.gz";
    sha256 = "05gr331bfhrv1zfb23r6sal14qyxxwj0qi4pdyg4s2bzx0w9apcd";
  };

  installPhase = ''
    mkdir $out
    tar -f $src -C $out -xz
    
    cd $out/yacy
    sed -i "/YACY_PARENT_DATA_PATH=..dirname/d" ./startYACY.sh
    sed -i "/cd .dirname/d" ./stopYACY.sh
    ln -s /var/lib/yacy DATA
    cat << EOF > ./env.sh
      JAVA_ARGS="-server -Xss256k -XX:MaxPermSize=256m -XX:ReservedCodeCacheSize=1024m -XX:+UseConcMarkSweepGC -XX:-UseGCOverheadLimit -XX:+UseAdaptiveSizePolicy -Djava.net.preferIPv4Stack=true -Djava.awt.headless=true -Dfile.encoding=UTF-8"
      HUGEPAGESTOTAL="\$(cat /proc/meminfo | grep HugePages_Total | sed s/[^0-9]//g)"
      if [ -n "\$HUGEPAGESTOTAL" ] && [ \$HUGEPAGESTOTAL -ne 0 ]; then 
          JAVA_ARGS="\$JAVA_ARGS -XX:+UseLargePages"
      fi
      if [ -s DATA/SETTINGS/yacy.conf ]; then
          for i in Xmx Xms; do
              j=\$(grep javastart_\$i DATA/SETTINGS/yacy.conf | sed 's/^[^=]*=//')
              if [ -n \$j ]; then
                  JAVA_ARGS="-\$j \$JAVA_ARGS"
              fi
          done
          j=\$(grep javastart_priority DATA/SETTINGS/yacy.conf | sed 's/^[^=]*=//')
          if [ ! -z "\$j" ]; then
              if [ -n \$j ]; then
                  NICE_VAL=\$j
              fi
          fi
      else
          JAVA_ARGS="-Xmx120m -Xms120m $JAVA_ARGS"
      fi
      CP=""
      for jarname in lib/*.jar; do
          CP=\$CP:\$jarname
      done
      CLASSPATH=.:\$CP
    EOF
  '';

  phases = "installPhase";

  meta = {
    description = "Yacy Search Server";
    homepage    = "https://yacy.net";
    license     = stdenv.lib.licenses.gpl2;
    platforms   = stdenv.lib.platforms.unix;
    maintainers = [ ];
  };
}
