require('./css/index.scss')

const Elm = require('./Main.elm')

const app = Elm.Main.embed(document.querySelector('#main'))

let $editor

let mounted = false

app.ports.renderSelection.subscribe(state => {
  console.log('renderSelection', state)

  if (!mounted) {
    // first mount
    onEditorMount()
    mounted = true
  }

  window.requestAnimationFrame(() => {
    const range = document.createRange()

    if (!document.getElementById(state.anchorKey)) {
      return
    }

    const startNode = document.getElementById(state.anchorKey).childNodes[0]
    range.setStart(startNode, state.anchorOffset)

    const selection = window.getSelection()
    selection.removeAllRanges()
    selection.addRange(range)
  })
})

function onEditorMount() {
  $editor = document.querySelector('.editor')
  window.$editor = $editor

  $editor.addEventListener('keydown', e => {
    // arrow keys
    if ([37, 38, 39, 40].includes(e.keyCode)) {
      // HACK: arrow key needs to update selection, so don't preventDefault
      return
    }

    e.preventDefault()
  })
}

document.addEventListener('selectionchange', function() {
  const {
    anchorNode,
    focusNode,
    anchorOffset,
    focusOffset,
    isCollapsed,
  } = window.getSelection()

  // Ignore selections that happen outside of the editor
  if (!isDescendent(anchorNode, $editor)) {
    return
  }

  let isBackward = false

  const position = anchorNode.compareDocumentPosition(focusNode)
  if (position & 0x02) {
    // Block nodes are backward
    isBackward = true
  } else if (position === 0 && anchorOffset > focusOffset) {
    // Selection is within same block and offsets at backward
    isBackward = true
  }

  const state = {
    anchorKey: findBlockKey(anchorNode),
    focusKey: findBlockKey(focusNode),
    anchorOffset,
    focusOffset,
    isCollapsed,
    isBackward,
  }

  app.ports.selectionChange.send(state)
})

// Checks if child DOM node is a descendent of parent node
function isDescendent(child, parent) {
  let node = child.parentNode
  while (node) {
    if (node === parent) {
      return true
    }
    node = node.parentNode
  }
  return false
}

// Finds the block key that contains a node.
// Returns null if node is not contained in a block.
function findBlockKey(node) {
  while (node) {
    // Text nodes don't have classList so skip TEXT_NODEs
    if (
      node.nodeType === Node.ELEMENT_NODE &&
      node.classList.contains('block')
    ) {
      return node.id
    }
    node = node.parentNode
  }
  return null
}
