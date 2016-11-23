#!/bin/sh

cat >>/usr/share/X11/xkb/rules/evdev <<EOF

! model = types
  *     = +custom

! layout = symbols
  custom = custom
EOF

patch -d /usr/share/X11/xkb/rules -Np1 >/dev/null <<EOF
--- a/evdev.xml
+++ b/evdev.xml
@@ -1311,6 +1311,16 @@
     </model>
   </modelList>
   <layoutList>
+     <layout>
+       <configItem>
+        <name>custom</name>
+        <shortDescription>custom</shortDescription>
+        <description>English (US, Customized SpaceFN)</description>
+        <languageList>
+          <iso639Id>eng</iso639Id>
+        </languageList>
+      </configItem>
+    </layout>
     <layout>
       <configItem>
         <name>us</name>
EOF
