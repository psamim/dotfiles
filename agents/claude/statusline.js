#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const crypto = require('crypto');
const { execSync } = require('child_process');
const os = require('os');

const CACHE_DIR = path.join(os.homedir(), '.claude', 'barkeep-cache');
const GIT_CACHE_TTL = 10 * 1000; // 10 seconds
const DEFAULT_CONTEXT_SIZE = 200000;
const TRANSCRIPT_TAIL_BYTES = 256 * 1024;

// 24-bit ANSI color from a hex value
const rgb = (hex) => `\x1b[38;2;${parseInt(hex.slice(1, 3), 16)};${parseInt(hex.slice(3, 5), 16)};${parseInt(hex.slice(5, 7), 16)}m`;

// Catppuccin Mocha colors
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  rosewater: rgb('#f5e0dc'),
  flamingo: rgb('#f2cdcd'),
  mauve: rgb('#cba6f7'),
  red: rgb('#f38ba8'),
  peach: rgb('#fab387'),
  yellow: rgb('#f9e2af'),
  green: rgb('#a6e3a1'),
  teal: rgb('#94e2d5'),
  sapphire: rgb('#74c7ec'),
  blue: rgb('#89b4fa'),
  lavender: rgb('#b4befe'),
  overlay0: rgb('#6c7086'),
  overlay2: rgb('#9399b2')
};

const separator = ` ${colors.overlay2}\u00B7${colors.reset} `;

// Nerd Font icons
const icons = {
  model: '\u{F06A9}', // md-robot
  folder: '\u{F413}', // oct-file_directory
  worktree: '\u{F0645}', // md-file_tree
  branch: '\u{E725}', // dev-git_branch
  files: '\u{F4A5}', // oct-file
  diff: '\u{F440}', // oct-diff
  cost: '\u{F0114}', // md-cash
  duration: '\u{F051B}', // md-timer_outline
  context: '\u{F029A}' // md-gauge
};

// Ensure cache directory exists
function ensureCacheDir() {
  try {
    if (!fs.existsSync(CACHE_DIR)) {
      fs.mkdirSync(CACHE_DIR, { recursive: true });
    }
  } catch (error) {
    // Silent fail if we can't create cache directory
  }
}

// Read from cache with TTL check
function readCache(filename, ttl) {
  try {
    const cachePath = path.join(CACHE_DIR, filename);
    if (!fs.existsSync(cachePath)) return null;

    const stats = fs.statSync(cachePath);
    const age = Date.now() - stats.mtime.getTime();

    if (age > ttl) {
      fs.unlinkSync(cachePath);
      return null;
    }

    return JSON.parse(fs.readFileSync(cachePath, 'utf8'));
  } catch (error) {
    return null;
  }
}

// Write to cache
function writeCache(filename, data) {
  try {
    const cachePath = path.join(CACHE_DIR, filename);
    fs.writeFileSync(cachePath, JSON.stringify(data));
  } catch (error) {
    // Silent fail if we can't write to cache
  }
}

function runGit(args, cwd) {
  return execSync(`git ${args}`, {
    cwd,
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'ignore'],
    env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' } // prevent locking to avoid this issue: https://github.com/anthropics/claude-code/issues/11005
  }).trim();
}

// Get git information with caching, one cache file per directory
function getGitInfo(cwd) {
  const cacheFile = `git-${crypto.createHash('md5').update(cwd).digest('hex')}.json`;
  const cached = readCache(cacheFile, GIT_CACHE_TTL);
  if (cached) return cached;

  try {
    const [gitDir, commonDir, topLevel] = runGit(
      'rev-parse --path-format=absolute --git-dir --git-common-dir --show-toplevel',
      cwd
    ).split('\n');

    // A linked worktree has its own git dir, separate from the main repo's
    const worktree = gitDir !== commonDir ? path.basename(topLevel) : null;
    const repoName = path.basename(path.dirname(commonDir));

    const status = runGit('status --porcelain=v2 --branch', cwd);

    let branch = 'HEAD';
    let oid = '';
    let ahead = 0;
    let behind = 0;
    let changedFiles = 0;

    for (const line of status.split('\n')) {
      if (line.startsWith('# branch.head ')) {
        branch = line.slice('# branch.head '.length);
      } else if (line.startsWith('# branch.oid ')) {
        oid = line.slice('# branch.oid '.length);
      } else if (line.startsWith('# branch.ab ')) {
        const [, a, b] = line.match(/\+(\d+) -(\d+)/) || [];
        ahead = Number(a) || 0;
        behind = Number(b) || 0;
      } else if (line && !line.startsWith('#')) {
        changedFiles++;
      }
    }

    if (branch === '(detached)') {
      branch = oid.slice(0, 7);
    }

    const gitInfo = {
      branch,
      isDirty: changedFiles > 0,
      changedFiles,
      ahead,
      behind,
      repoName,
      worktree
    };
    writeCache(cacheFile, gitInfo);
    return gitInfo;
  } catch (error) {
    const fallback = { branch: 'no-git', isDirty: false, changedFiles: 0, ahead: 0, behind: 0 };
    writeCache(cacheFile, fallback);
    return fallback;
  }
}

// Format model name with appropriate coloring and icon
function formatModel(model) {
  if (!model) return `${colors.overlay0}${icons.model} unknown${colors.reset}`;

  const modelLower = model.toLowerCase();

  if (modelLower.includes('opus')) {
    return `${colors.mauve}${icons.model} ${model}${colors.reset}`;
  } else if (modelLower.includes('sonnet')) {
    return `${colors.sapphire}${icons.model} ${model}${colors.reset}`;
  } else if (modelLower.includes('haiku')) {
    return `${colors.green}${icons.model} ${model}${colors.reset}`;
  } else {
    return `${colors.teal}${icons.model} ${model}${colors.reset}`;
  }
}

// Format cost with appropriate coloring
function formatCost(cost) {
  if (typeof cost !== 'number' || cost === 0) {
    return `${colors.overlay0}${icons.cost} $0.00${colors.reset}`;
  }

  const formatted = cost.toFixed(2);

  if (cost > 10) {
    return `${colors.red}${colors.bright}${icons.cost} $${formatted}${colors.reset}`;
  } else if (cost > 1) {
    return `${colors.peach}${icons.cost} $${formatted}${colors.reset}`;
  } else {
    return `${colors.flamingo}${icons.cost} $${formatted}${colors.reset}`;
  }
}

function sumInputTokens(usage) {
  return (usage.input_tokens || 0) +
    (usage.cache_read_input_tokens || 0) +
    (usage.cache_creation_input_tokens || 0);
}

// Read the latest usage from the end of the transcript file
function getTranscriptInputTokens(transcriptPath) {
  if (!transcriptPath) return 0;

  try {
    const fd = fs.openSync(transcriptPath, 'r');
    const size = fs.fstatSync(fd).size;
    const length = Math.min(size, TRANSCRIPT_TAIL_BYTES);
    const buffer = Buffer.alloc(length);
    fs.readSync(fd, buffer, 0, length, size - length);
    fs.closeSync(fd);

    const lines = buffer.toString('utf8').split('\n').reverse();
    for (const line of lines) {
      try {
        const entry = JSON.parse(line);
        if (entry.type === 'assistant' && entry.message && entry.message.usage) {
          return sumInputTokens(entry.message.usage);
        }
      } catch (lineError) {
        // Skip invalid or cut-off lines
      }
    }
  } catch (error) {
    // Silent fail if the transcript can't be read
  }

  return 0;
}

// Use the context window data from Claude Code, fall back to the transcript
function getContextPercentage(data) {
  const contextWindow = data.context_window || {};

  if (typeof contextWindow.used_percentage === 'number') {
    return contextWindow.used_percentage;
  }

  const maxTokens = contextWindow.context_window_size || DEFAULT_CONTEXT_SIZE;
  const inputTokens = contextWindow.current_usage
    ? sumInputTokens(contextWindow.current_usage)
    : getTranscriptInputTokens(data.transcript_path);

  return (inputTokens / maxTokens) * 100;
}

// Format context percentage
function formatContext(contextPercentage) {
  if (typeof contextPercentage !== 'number' || contextPercentage === 0) {
    return `${colors.overlay0}${icons.context} 0%${colors.reset}`;
  }

  // Show one decimal place for precision
  const percentage = contextPercentage.toFixed(1);

  if (contextPercentage > 90) {
    return `${colors.red}${colors.bright}${icons.context} ${percentage}%${colors.reset}`;
  } else if (contextPercentage > 70) {
    return `${colors.yellow}${icons.context} ${percentage}%${colors.reset}`;
  } else {
    return `${colors.lavender}${icons.context} ${percentage}%${colors.reset}`;
  }
}

// Format session duration
function formatDuration(durationMs) {
  if (!durationMs || durationMs === 0) {
    return `${colors.overlay0}${icons.duration} 0s${colors.reset}`;
  }

  const seconds = Math.floor(durationMs / 1000);
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);

  let display;
  if (hours > 0) {
    display = `${hours}h ${minutes % 60}m`;
  } else if (minutes > 0) {
    display = `${minutes}m ${seconds % 60}s`;
  } else {
    display = `${seconds}s`;
  }

  return `${colors.sapphire}${icons.duration} ${display}${colors.reset}`;
}

// Format lines changed (added/removed)
function formatLinesChanged(linesAdded, linesRemoved, isDirty) {
  const hasAdded = linesAdded && linesAdded > 0;
  const hasRemoved = linesRemoved && linesRemoved > 0;

  // If there are actual changes, show them
  if (hasAdded || hasRemoved) {
    const parts = [];
    if (hasAdded) {
      parts.push(`${colors.green}+${linesAdded}${colors.reset}`);
    }
    if (hasRemoved) {
      parts.push(`${colors.red}-${linesRemoved}${colors.reset}`);
    }
    const iconColor = hasAdded ? colors.green : colors.red;
    return `${iconColor}${icons.diff}${colors.reset} ${parts.join('/')}`;
  }

  // If repo is dirty but no line changes tracked, show indicator
  if (isDirty) {
    return `${colors.overlay0}${icons.diff} ~${colors.reset}`;
  }

  // No changes at all
  return '';
}

// Format changed files count
function formatChangedFiles(changedFiles) {
  if (!changedFiles) return '';
  const label = changedFiles === 1 ? 'file' : 'files';
  return `${colors.rosewater}${icons.files} ${changedFiles} ${label}${colors.reset}`;
}

// Format commits ahead/behind upstream
function formatAheadBehind(ahead, behind) {
  const parts = [];
  if (ahead > 0) parts.push(`${colors.green}↑${ahead}${colors.reset}`);
  if (behind > 0) parts.push(`${colors.red}↓${behind}${colors.reset}`);
  return parts.join(' ');
}

// Format directory: repo name, plus the worktree folder in a linked worktree
function formatDirectory(cwd, gitInfo) {
  if (!cwd) return `${colors.overlay0}${icons.folder} unknown${colors.reset}`;

  if (gitInfo.worktree) {
    return `${colors.teal}${icons.folder} ${gitInfo.repoName} ${icons.worktree} ${gitInfo.worktree}${colors.reset}`;
  }

  return `${colors.teal}${icons.folder} ${path.basename(cwd)}${colors.reset}`;
}

// Main statusline generation
function generateStatusLine(data) {
  ensureCacheDir();

  const cwd = data.workspace?.current_dir || data.cwd || process.cwd();
  const gitInfo = getGitInfo(cwd);

  // Extract model name (handle both old and new format)
  const baseModelName = data.model?.display_name || data.model?.id || data.model;
  const effort = data.effort?.level;
  const modelName = effort && baseModelName ? `${baseModelName} (${effort})` : baseModelName;

  // Extract cost (handle both old and new format)
  const costValue = data.cost?.total_cost_usd ?? data.cost ?? 0;

  // Extract duration and lines changed
  const durationMs = data.cost?.total_duration_ms ?? 0;
  const linesAdded = data.cost?.total_lines_added ?? 0;
  const linesRemoved = data.cost?.total_lines_removed ?? 0;

  const dirtyIndicator = gitInfo.isDirty ? '*' : '';
  const gitDisplay = [
    `${colors.blue}${icons.branch} ${gitInfo.branch}${dirtyIndicator}${colors.reset}`,
    formatAheadBehind(gitInfo.ahead, gitInfo.behind),
    formatChangedFiles(gitInfo.changedFiles),
    formatLinesChanged(linesAdded, linesRemoved, gitInfo.isDirty)
  ].filter(Boolean).join(' ');

  // Line 1: Model, Directory, Branch with changes
  const line1 = [
    formatModel(modelName),
    formatDirectory(cwd, gitInfo),
    gitDisplay
  ].join(separator);

  // Line 2: Cost, Duration, Context
  const line2 = [
    formatCost(costValue),
    formatDuration(durationMs),
    formatContext(getContextPercentage(data))
  ].join(separator);

  return `${line1}\n${line2}`;
}

// Handle graceful errors
function handleError() {
  const fallbackData = {
    model: { id: 'unknown', display_name: 'unknown' },
    cwd: process.cwd(),
    cost: {
      total_cost_usd: 0,
      total_duration_ms: 0,
      total_lines_added: 0,
      total_lines_removed: 0
    },
    transcript_path: null
  };

  return generateStatusLine(fallbackData);
}

// Main execution
function main() {
  try {
    let input = '';

    if (process.stdin.isTTY) {
      // If no stdin, provide test data
      const testData = {
        model: { id: 'claude-opus-5-5', display_name: 'Opus 5.5' },
        cwd: process.cwd(),
        cost: {
          total_cost_usd: 2.47,
          total_duration_ms: 135000,
          total_lines_added: 127,
          total_lines_removed: 11
        },
        context_window: { used_percentage: 42 },
        transcript_path: null
      };
      console.log(generateStatusLine(testData));
      return;
    }

    // Read from stdin
    process.stdin.setEncoding('utf8');

    process.stdin.on('readable', () => {
      const chunk = process.stdin.read();
      if (chunk !== null) {
        input += chunk;
      }
    });

    process.stdin.on('end', () => {
      try {
        const data = JSON.parse(input);
        console.log(generateStatusLine(data));
      } catch (parseError) {
        console.log(handleError());
      }
    });

    // Handle stdin errors
    process.stdin.on('error', () => {
      console.log(handleError());
    });

  } catch (error) {
    console.log(handleError());
  }
}

// Handle uncaught exceptions
process.on('uncaughtException', () => {
  console.log(handleError());
  process.exit(0);
});

process.on('unhandledRejection', () => {
  console.log(handleError());
  process.exit(0);
});

main();
