#!/usr/bin/env bash
set -euo pipefail

###############################################################################
# Configuration – change these if needed
###############################################################################

# Where Kanata binary lives (Homebrew default on Apple Silicon)
KANATA_PATH="/opt/homebrew/bin/kanata"

# Your Kanata config file
KANATA_CFG_PATH="${HOME}/.dotfiles/kanata/kanata-mac.kbd"

# Karabiner VirtualHIDDevice paths (from standalone pkg install)
VHID_DAEMON="/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice/Applications/Karabiner-VirtualHIDDevice-Daemon.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Daemon"
VHID_MANAGER="/Applications/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager"

# LaunchDaemon plist paths
KANATA_PLIST="/Library/LaunchDaemons/com.jtroo.kanata.plist"
VHID_DAEMON_PLIST="/Library/LaunchDaemons/com.pqrs.karabiner.vhid-daemon.plist"
VHID_MANAGER_PLIST="/Library/LaunchDaemons/com.pqrs.karabiner.vhid-manager.plist"

# Log files
LOG_DIR="/var/log/kanata"
mkdir -p "${LOG_DIR}"
STDOUT_LOG="${LOG_DIR}/kanata.output.log"
STDERR_LOG="${LOG_DIR}/kanata.error.log"
VHID_DAEMON_LOG="${LOG_DIR}/vhid-daemon.log"

LABEL_KANATA="com.jtroo.kanata"
LABEL_VHID_DAEMON="com.pqrs.karabiner.vhid-daemon"
LABEL_VHID_MANAGER="com.pqrs.karabiner.vhid-manager"

###############################################################################
# Checks
###############################################################################

if [[ ! -x "${KANATA_PATH}" ]]; then
  echo "ERROR: Kanata binary not found or not executable at: ${KANATA_PATH}"
  echo "Install with Homebrew (brew install kanata) or update KANATA_PATH."
  exit 1
fi

if [[ ! -f "${KANATA_CFG_PATH}" ]]; then
  echo "ERROR: Kanata config not found at: ${KANATA_CFG_PATH}"
  echo "Create your config and/or update KANATA_CFG_PATH."
  exit 1
fi

if [[ ! -x "${VHID_DAEMON}" ]]; then
  echo "ERROR: Karabiner VirtualHIDDevice Daemon not found at: ${VHID_DAEMON}"
  echo "Install standalone Karabiner-DriverKit-VirtualHIDDevice pkg v5.0.0+:"
  echo "https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice/releases"
  exit 1
fi

if [[ ! -x "${VHID_MANAGER}" ]]; then
  echo "WARNING: Manager not found at ${VHID_MANAGER}. Run manually if needed:"
  echo "sudo ${VHID_MANAGER} activate"
fi

if [[ $EUID -ne 0 ]]; then
  echo "This script will now re‑run itself with sudo..."
  exec sudo "$0" "$@"
fi

###############################################################################
# Create log files
###############################################################################

touch "${STDOUT_LOG}" "${STDERR_LOG}" "${VHID_DAEMON_LOG}"
chmod 644 "${STDOUT_LOG}" "${STDERR_LOG}" "${VHID_DAEMON_LOG}"

###############################################################################
# Create Karabiner VHID Daemon plist
###############################################################################

cat > "${VHID_DAEMON_PLIST}" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>${LABEL_VHID_DAEMON}</string>
    <key>ProgramArguments</key>
    <array>
        <string>${VHID_DAEMON}</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>
    <key>ProcessType</key>
    <string>Interactive</string>
    <key>StandardOutPath</key>
    <string>${VHID_DAEMON_LOG}</string>
    <key>StandardErrorPath</key>
    <string>${VHID_DAEMON_LOG}</string>
</dict>
</plist>
EOF

###############################################################################
# Create Karabiner VHID Manager plist (activates driver)
###############################################################################

cat > "${VHID_MANAGER_PLIST}" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>${LABEL_VHID_MANAGER}</string>
    <key>ProgramArguments</key>
    <array>
        <string>${VHID_MANAGER}</string>
        <string>activate</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
    <key>StandardErrorPath</key>
    <string>${LOG_DIR}/vhid-manager.log</string>
</dict>
</plist>
EOF

###############################################################################
# Create Kanata plist (updated with --port for stability)
###############################################################################

cat > "${KANATA_PLIST}" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>${LABEL_KANATA}</string>

    <key>ProgramArguments</key>
    <array>
        <string>${KANATA_PATH}</string>
        <string>-c</string>
        <string>${KANATA_CFG_PATH}</string>
        <string>-n</string>
        <string>--port</string>
        <string>10000</string>
    </array>

    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>

    <key>StandardOutPath</key>
    <string>${STDOUT_LOG}</string>
    <key>StandardErrorPath</key>
    <string>${STDERR_LOG}</string>
</dict>
</plist>
EOF

# Set permissions
chmod 644 "${KANATA_PLIST}" "${VHID_DAEMON_PLIST}" "${VHID_MANAGER_PLIST}"
chown root:wheel "${KANATA_PLIST}" "${VHID_DAEMON_PLIST}" "${VHID_MANAGER_PLIST}"

# Validate plists
plutil "${KANATA_PLIST}" "${VHID_DAEMON_PLIST}" "${VHID_MANAGER_PLIST}"

###############################################################################
# Register all services with launchd
###############################################################################

PLISTS=("${KANATA_PLIST}" "${VHID_DAEMON_PLIST}" "${VHID_MANAGER_PLIST}")
LABELS=("${LABEL_KANATA}" "${LABEL_VHID_DAEMON}" "${LABEL_VHID_MANAGER}")

for i in "${!PLISTS[@]}"; do
  PLIST="${PLISTS[$i]}"
  LABEL="${LABELS[$i]}"
  launchctl bootout system "${PLIST}" 2>/dev/null || true
  launchctl bootstrap system "${PLIST}"
  launchctl enable "system/${LABEL}"
  echo "Enabled: ${LABEL}"
done

echo "=== COMPLETE ==="
echo "Full stack installed:"
echo "- VHID Manager: ${LABEL_VHID_MANAGER}"
echo "- VHID Daemon:  ${LABEL_VHID_DAEMON}"
echo "- Kanata:       ${LABEL_KANATA}"
echo ""
echo "Logs:"
echo "  tail -f ${STDOUT_LOG} ${STDERR_LOG} ${VHID_DAEMON_LOG}"
echo ""
echo "Verify all running:"
echo "  sudo launchctl list | grep -E '(vhid|kanata)'"
echo ""
echo "To uninstall:"
echo "  sudo launchctl bootout system ${PLISTS[*]}"
echo "  sudo rm ${PLISTS[*]}"
echo "Reboot recommended to apply all TCC permissions!"
