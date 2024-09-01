{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressRichEditControl }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxRichEdit.Commands.Strs;

{$I cxVer.inc}

interface

resourcestring

  sdxRichEditCommandAddParagraphsToTableOfContentsDescription =
    'Add the current paragraph as an entry in the Table of Contents.';
  sdxRichEditCommandAddParagraphsToTableOfContentsMenuCaption = 'Add Text';
  sdxRichEditCommandAddWordToDictionaryDescription = 'Add to Dictionary';
  sdxRichEditCommandAddWordToDictionaryMenuCaption = 'Add to Dictionary';
  sdxRichEditCommandAutoCorrectPlaceholderDescription = 'AutoCorrect';
  sdxRichEditCommandAutoCorrectPlaceholderMenuCaption = 'AutoCorrect';
  sdxRichEditCommandBackSpaceKeyDescription = 'BackSpaceKey';
  sdxRichEditCommandBackSpaceKeyMenuCaption = 'BackSpaceKey';
  sdxRichEditCommandBookmarkDescription = 'Bookmark...';
  sdxRichEditCommandBookmarkMenuCaption = 'Bookmark...';
  sdxRichEditCommandCapitalizeEachWordTextCaseDescription =
    'Capitalize each word.';
  sdxRichEditCommandCapitalizeEachWordTextCaseMenuCaption =
    'Capitalize Each Word';
  sdxRichEditCommandChangeColumnSizeDescription = 'ChangeColumnSizeDescription';
  sdxRichEditCommandChangeColumnSizeMenuCaption = 'ChangeColumnSize';
  sdxRichEditCommandChangeFloatingObjectAlignmentDescription =
    'Position the selected object on the page. Text is automatically set to wrap around the object.';
  sdxRichEditCommandChangeFloatingObjectAlignmentMenuCaption = 'Position';
  sdxRichEditCommandChangeFloatingObjectFillColorDescription =
    'Fill the selected shape with a solid color.';
  sdxRichEditCommandChangeFloatingObjectFillColorMenuCaption = 'Shape Fill';
  sdxRichEditCommandChangeFloatingObjectOutlineColorDescription =
    'Specify color for the outline of the selected shape.';
  sdxRichEditCommandChangeFloatingObjectOutlineColorMenuCaption =
    'Shape Outline';
  sdxRichEditCommandChangeFloatingObjectOutlineWidthDescription =
    'Specify width for the outline of the selected shape.';
  sdxRichEditCommandChangeFloatingObjectOutlineWidthMenuCaption =
    'Shape Outline Weight';
  sdxRichEditCommandChangeFloatingObjectTextWrapTypeDescription =
    'Change the way text wraps around the selected object. To configure the object so that it moves along with text the text around it, select \"In Line With Text\".';
  sdxRichEditCommandChangeFloatingObjectTextWrapTypeMenuCaption = 'Wrap Text';
  sdxRichEditCommandChangeFontColorDescription = 'Change the font color.';
  sdxRichEditCommandChangeFontColorMenuCaption = 'Font Color';
  sdxRichEditCommandChangeFontNameDescription = 'Change the font face.';
  sdxRichEditCommandChangeFontNameMenuCaption = 'Font';
  sdxRichEditCommandChangeFontSizeDescription = 'Change the font size.';
  sdxRichEditCommandChangeFontSizeMenuCaption = 'Font Size';
  sdxRichEditCommandChangeIndentDescription = 'ChangeIndentDescription';
  sdxRichEditCommandChangeIndentMenuCaption = 'ChangeIndent';
  sdxRichEditCommandChangeMistakenWordDescription = '(no spelling suggestions)';
  sdxRichEditCommandChangeMistakenWordMenuCaption = '(no spelling suggestions)';
  sdxRichEditCommandChangePageColorDescription =
    'Choose a color for the background of the page.';
  sdxRichEditCommandChangePageColorMenuCaption = 'Page Color';
  sdxRichEditCommandChangeParagraphFirstLineIndentDescription =
    'IncrementParagraphFirstLineIndentDescription';
  sdxRichEditCommandChangeParagraphFirstLineIndentMenuCaption =
    'IncrementParagraphFirstLineIndent';
  sdxRichEditCommandChangeParagraphLeftIndentDescription =
    'ChangeParagraphLeftIndentDescription';
  sdxRichEditCommandChangeParagraphLeftIndentMenuCaption =
    'ChangeParagraphLeftIndent';
  sdxRichEditCommandChangeParagraphRightIndentDescription =
    'ChangeParagraphRightIndentDescription';
  sdxRichEditCommandChangeParagraphRightIndentMenuCaption =
    'ChangeParagraphRightIndent';
  sdxRichEditCommandChangeParagraphStyleDescription = 'ChangeParagraphStyle';
  sdxRichEditCommandChangeParagraphStyleMenuCaption = 'ChangeParagraphStyle';
  sdxRichEditCommandChangeSectionLineNumberingDescription =
    'Add line numbers in the margin alongside of each line of the document.';
  sdxRichEditCommandChangeSectionLineNumberingMenuCaption = 'Line Numbers';
  sdxRichEditCommandChangeSectionPagePaperKindDescription =
    'Choose a paper size for the current section.';
  sdxRichEditCommandChangeSectionPagePaperKindMenuCaption = 'Size';
  sdxRichEditCommandChangeTableCellsContentAlignmentDescription =
    'Cell Alignment';
  sdxRichEditCommandChangeTableCellsContentAlignmentMenuCaption =
    'Cell Alignment';
  sdxRichEditCommandChangeTextCaseDescription =
    'Change all the selected text to UPPERCASE, lowercase, or other common capitalizations.';
  sdxRichEditCommandChangeTextCaseMenuCaption = 'Change Case';
  sdxRichEditCommandClearFormattingMenuCaption = 'Clear Formatting';
  sdxRichEditCommandClearUndoDescription = 'Clear Undo Buffer';
  sdxRichEditCommandClearUndoMenuCaption = 'ClearUndo';
  sdxRichEditCommandClosePageHeaderFooterDescription =
    'Close the Header and Footer Tools.'#13#10#13#10'You can also double click the document area to return to editing it.';
  sdxRichEditCommandClosePageHeaderFooterMenuCaption =
    'Close Header and Footer';
  sdxRichEditCommandCopySelectionDescription =
    'Copy the selection and put it on the Clipboard.';
  sdxRichEditCommandCopySelectionMenuCaption = '&Copy';
  sdxRichEditCommandCreateBookmarkDescription = 'Create Bookmark';
  sdxRichEditCommandCreateBookmarkMenuCaption = 'Create Bookmark';
  sdxRichEditCommandCreateFieldDescription = 'Create Field';
  sdxRichEditCommandCreateFieldMenuCaption = 'Create Field';
  sdxRichEditCommandCreateHyperlinkDescription = 'CreateHyperlinkDescription';
  sdxRichEditCommandCreateHyperlinkMenuCaption = 'CreateHyperlink';
  sdxRichEditCommandCutSelectionDescription =
    'Cut the selection from the document and put it on the Clipboard.';
  sdxRichEditCommandCutSelectionMenuCaption = 'Cu&t';
  sdxRichEditCommandDecreaseFontSizeDescription = 'Decrease the font size.';
  sdxRichEditCommandDecreaseFontSizeMenuCaption = 'Shrink Font';
  sdxRichEditCommandDecrementFontSizeDescription = 'DecrementFontSize';
  sdxRichEditCommandDecrementFontSizeMenuCaption = 'DecrementFontSize';
  sdxRichEditCommandDecrementIndentDescription =
    'Decrease the indent level of the paragraph.';
  sdxRichEditCommandDecrementIndentMenuCaption = 'Decrease Indent';
  sdxRichEditCommandDecrementNumerationFromParagraphDescription =
    'DecrementNumerationFromParagraph';
  sdxRichEditCommandDecrementNumerationFromParagraphMenuCaption =
    'DecrementNumerationFromParagraph';
  sdxRichEditCommandDecrementParagraphLeftIndentDescription =
    'DecrementParagraphLeftIndentDescription';
  sdxRichEditCommandDecrementParagraphLeftIndentMenuCaption =
    'DecrementParagraphLeftIndent';
  sdxRichEditCommandDecrementParagraphOutlineLevelDescription =
    'Decrement outline level';
  sdxRichEditCommandDecrementParagraphOutlineLevelMenuCaption =
    'Decrement outline level';
  sdxRichEditCommandDeleteBackCoreDescription = 'DeleteBackCoreDescription';
  sdxRichEditCommandDeleteBackCoreMenuCaption = 'DeleteBackCore';
  sdxRichEditCommandDeleteBackDescription = 'DeleteBack';
  sdxRichEditCommandDeleteBackMenuCaption = 'DeleteBack';
  sdxRichEditCommandDeleteBookmarkDescription = 'Delete Bookmark';
  sdxRichEditCommandDeleteBookmarkMenuCaption = 'Delete Bookmark';
  sdxRichEditCommandDeleteDescription = 'Delete';
  sdxRichEditCommandDeleteMenuCaption = 'Delete';
  sdxRichEditCommandDeleteNumerationFromParagraphDescription =
    'DeleteNumerationFromParagraph';
  sdxRichEditCommandDeleteNumerationFromParagraphMenuCaption =
    'DeleteNumerationFromParagraph';
  sdxRichEditCommandDeleteRepeatedWordDescription = 'Delete Repeated Word';
  sdxRichEditCommandDeleteRepeatedWordMenuCaption = 'Delete Repeated Word';
  sdxRichEditCommandDeleteTableCellsDescription =
    'Delete rows, columns, or cells.';
  sdxRichEditCommandDeleteTableCellsMenuCaption = 'Delete Cells...';
  sdxRichEditCommandDeleteTableCellsMenuItem = 'Delete Cells...';
  sdxRichEditCommandDeleteTableColumnsDescription = 'Delete Columns';
  sdxRichEditCommandDeleteTableColumnsMenuCaption = 'Delete Columns';
  sdxRichEditCommandDeleteTableDescription = 'Delete Entire Table.';
  sdxRichEditCommandDeleteTableMenuCaption = 'Delete Table';
  sdxRichEditCommandDeleteTableRowsDescription = 'Delete Rows';
  sdxRichEditCommandDeleteTableRowsMenuCaption = 'Delete Rows';
  sdxRichEditCommandDeleteWordBackCoreDescription = 'DeleteWordBack';
  sdxRichEditCommandDeleteWordBackCoreMenuCaption = 'DeleteWordBack';
  sdxRichEditCommandDeleteWordBackDescription = 'DeleteWordBack';
  sdxRichEditCommandDeleteWordBackMenuCaption = 'DeleteWordBack';
  sdxRichEditCommandDeleteWordCoreDescription = 'DeleteWordCoreDescription';
  sdxRichEditCommandDeleteWordCoreMenuCaption = 'DeleteWordCore';
  sdxRichEditCommandDeleteWordDescription = 'DeleteWord';
  sdxRichEditCommandDeleteWordMenuCaption = 'DeleteWord';
  sdxRichEditCommandDeselectAllDescription = 'Reset document selection.';
  sdxRichEditCommandDeselectAllMenuCaption = 'Deselect All';
  sdxRichEditCommandEditHyperlinkDescription = 'Edit hyperlink...';
  sdxRichEditCommandEditHyperlinkMenuCaption = 'Edit hyperlink...';
  sdxRichEditCommandEditPageFooterDescription =
    'Edit the footer of the document.'#13#10#13#10'The content in the footer will appear at the bottom of each printed page.';
  sdxRichEditCommandEditPageFooterMenuCaption = 'Footer';
  sdxRichEditCommandEditPageHeaderDescription =
    'Edit the header of the document.'#13#10#13#10'The content in the header will appear at the top of each printed page.';
  sdxRichEditCommandEditPageHeaderMenuCaption = 'Header';
  sdxRichEditCommandEditTOCMenuCaption = 'Edit Table of Contents...';
  sdxRichEditCommandEncryptDocumentDescription =
    'Password-protect this document.';
  sdxRichEditCommandEncryptDocumentMenuCaption = 'Encrypt with Password';
  sdxRichEditCommandEnsureCaretVisibleHorizontallyDescription =
    'EnsureCaretVisibleHorizontally';
  sdxRichEditCommandEnsureCaretVisibleHorizontallyMenuCaption =
    'EnsureCaretVisibleHorizontally';
  sdxRichEditCommandEnsureCaretVisibleVerticallyDescription =
    'EnsureCaretVisibleVertically';
  sdxRichEditCommandEnsureCaretVisibleVerticallyMenuCaption =
    'EnsureCaretVisibleVertically';
  sdxRichEditCommandEnterKeyDescription = 'EnterKeyDescription';
  sdxRichEditCommandEnterKeyMenuCaption = 'EnterKey';
  sdxRichEditCommandFindAndSelectBackwardDescription = 'FindAndSelectBackward';
  sdxRichEditCommandFindAndSelectBackwardMenuCaption = 'FindAndSelectBackward';
  sdxRichEditCommandFindAndSelectForwardDescription = 'FindAndSelectForward';
  sdxRichEditCommandFindAndSelectForwardMenuCaption = 'FindAndSelectForward';
  sdxRichEditCommandFindDescription = 'Find text in the document.';
  sdxRichEditCommandFindMenuCaption = 'Find';
  sdxRichEditCommandFindNextDescription = 'Repeats the last search forward.';
  sdxRichEditCommandFindNextMenuCaption = 'Find Next';
  sdxRichEditCommandFindPrevDescription = 'Repeats the last search backward.';
  sdxRichEditCommandFindPrevMenuCaption = 'Find Prev';
  sdxRichEditCommandFloatingObjectBringForwardDescription =
    'Bring the selected object forward so that it is hidden by fewer object that are in front of it.';
  sdxRichEditCommandFloatingObjectBringForwardMenuCaption = 'Bring Forward';
  sdxRichEditCommandFloatingObjectBringForwardPlaceholderDescription =
    'Bring the selected object forward so that it is hidden by fewer object that are in front of it.';
  sdxRichEditCommandFloatingObjectBringForwardPlaceholderMenuCaption =
    'Bring to Front';
  sdxRichEditCommandFloatingObjectBringInFrontOfTextDescription = '';
  sdxRichEditCommandFloatingObjectBringInFrontOfTextMenuCaption =
    'Bring in Front of Text';
  sdxRichEditCommandFloatingObjectBringToFrontDescription =
    'Bring the selected object in front of all other objects so that no part of it is hidden behind another object.';
  sdxRichEditCommandFloatingObjectBringToFrontMenuCaption = 'Bring to Front';
  sdxRichEditCommandFloatingObjectSendBackwardDescription =
    'Send the selected object backward so that it is hidden by the object that are in front of it.';
  sdxRichEditCommandFloatingObjectSendBackwardMenuCaption = 'Send Backward';
  sdxRichEditCommandFloatingObjectSendBackwardPlaceholderDescription =
    'Send the selected object backward so that it is hidden by the object that are in front of it.';
  sdxRichEditCommandFloatingObjectSendBackwardPlaceholderMenuCaption =
    'Send to Back';
  sdxRichEditCommandFloatingObjectSendBehindTextDescription = '';
  sdxRichEditCommandFloatingObjectSendBehindTextMenuCaption =
    'Send Behind Text';
  sdxRichEditCommandFloatingObjectSendToBackDescription =
    'Send the selected object behind all other objects.';
  sdxRichEditCommandFloatingObjectSendToBackMenuCaption = 'Send to Back';
  sdxRichEditCommandFontSubscriptDescription =
    'Create small letters below the text baseline.';
  sdxRichEditCommandFontSubscriptMenuCaption = 'Subscript';
  sdxRichEditCommandFontSuperscriptDescription =
    'Create small letters above the line of text.';
  sdxRichEditCommandFontSuperscriptMenuCaption = 'Superscript';
  sdxRichEditCommandGoToNextHeaderFooterDescription =
    'Navigate to the next section''s header or footer.';
  sdxRichEditCommandGoToNextHeaderFooterMenuCaption = 'Show Next';
  sdxRichEditCommandGoToPageFooterDescription =
    'Activate the footer on this page so that you can edit it.';
  sdxRichEditCommandGoToPageFooterMenuCaption = 'Go to Footer';
  sdxRichEditCommandGoToPageHeaderDescription =
    'Activate the header on this page so that you can edit it.';
  sdxRichEditCommandGoToPageHeaderMenuCaption = 'Go to Header';
  sdxRichEditCommandGoToPreviousHeaderFooterDescription =
    'Navigate to the previous section''s header or footer.';
  sdxRichEditCommandGoToPreviousHeaderFooterMenuCaption = 'Show Previous';
  sdxRichEditCommandHighlightTextDescription =
    'Make text look like it was marked with a highlighter pen.';
  sdxRichEditCommandHighlightTextMenuCaption = 'Text Highlight Color';
  sdxRichEditCommandHyperlinkDescription = 'Hyperlink...';
  sdxRichEditCommandHyperlinkMenuCaption = 'Hyperlink...';
  sdxRichEditCommandIgnoreAllMistakenWordsDescription = 'Ignore All';
  sdxRichEditCommandIgnoreAllMistakenWordsMenuCaption = 'Ignore All';
  sdxRichEditCommandIgnoreMistakenWordDescription = 'Ignore';
  sdxRichEditCommandIgnoreMistakenWordMenuCaption = 'Ignore';
  sdxRichEditCommandIncreaseFontSizeDescription = 'Increase the font size.';
  sdxRichEditCommandIncreaseFontSizeMenuCaption = 'Grow Font';
  sdxRichEditCommandIncrementFontSizeDescription = 'IncrementFontSize';
  sdxRichEditCommandIncrementFontSizeMenuCaption = 'IncrementFontSize';
  sdxRichEditCommandIncrementIndentDescription =
    'Increase the indent level of the paragraph.';
  sdxRichEditCommandIncrementIndentMenuCaption = 'Increase Indent';
  sdxRichEditCommandIncrementNumerationFromParagraphDescription =
    'IncrementNumerationFromParagraph';
  sdxRichEditCommandIncrementNumerationFromParagraphMenuCaption =
    'IncrementNumerationFromParagraph';
  sdxRichEditCommandIncrementParagraphLeftIndentDescription =
    'IncrementParagraphLeftIndentDescription';
  sdxRichEditCommandIncrementParagraphLeftIndentMenuCaption =
    'IncrementParagraphLeftIndent';
  sdxRichEditCommandIncrementParagraphOutlineLevelDescription =
    'Increment outline level';
  sdxRichEditCommandIncrementParagraphOutlineLevelMenuCaption =
    'Increment outline level';
  sdxRichEditCommandInsertBreakDescription =
    'Add page, section, or column breaks to the document.';
  sdxRichEditCommandInsertBreakMenuCaption = 'Breaks';
  sdxRichEditCommandInsertBulletListDescription = 'Start a bulleted list.';
  sdxRichEditCommandInsertBulletListMenuCaption = 'Bullets';
  sdxRichEditCommandInsertCaptionPlaceholderDescription =
    'Add a caption to a picture or other image.'#13#10'A caption is a line of text that appears below an object to describe it.';
  sdxRichEditCommandInsertCaptionPlaceholderMenuCaption = 'Insert Caption';
  sdxRichEditCommandInsertColumnBreakDescription =
    'Indicate that the text following the column break will begin in the next column.';
  sdxRichEditCommandInsertColumnBreakMenuCaption = 'Column';
  sdxRichEditCommandInsertCopyrightSymbolDescription = 'InsertCopyrightSymbol';
  sdxRichEditCommandInsertCopyrightSymbolMenuCaption = 'InsertCopyrightSymbol';
  sdxRichEditCommandInsertEllipsisDescription = 'InsertEllipsis';
  sdxRichEditCommandInsertEllipsisMenuCaption = 'InsertEllipsis';
  sdxRichEditCommandInsertEmDashDescription = 'InsertEmDash';
  sdxRichEditCommandInsertEmDashMenuCaption = 'InsertEmDash';
  sdxRichEditCommandInsertEnDashDescription = 'InsertEnDash';
  sdxRichEditCommandInsertEnDashMenuCaption = 'InsertEnDash';
  sdxRichEditCommandInsertEquationCaptionDescription =
    'Add an equation caption.';
  sdxRichEditCommandInsertEquationCaptionMenuCaption = 'Equations Caption';
  sdxRichEditCommandInsertFieldDescription = 'InsertField';
  sdxRichEditCommandInsertFieldMenuCaption = 'InsertField';
  sdxRichEditCommandInsertFigureCaptionDescription = 'Add a figure caption.';
  sdxRichEditCommandInsertFigureCaptionMenuCaption = 'Figures Caption';
  sdxRichEditCommandInsertFloatingObjectPictureDescription =
    'Insert a picture from a file.';
  sdxRichEditCommandInsertFloatingObjectPictureMenuCaption = 'Picture';
  sdxRichEditCommandInsertHyperlinkDescription = 'InsertHyperlinkDescription';
  sdxRichEditCommandInsertHyperlinkMenuCaption = 'InsertHyperlink';
  sdxRichEditCommandInsertLineBreakDescription = 'InsertLineBreak';
  sdxRichEditCommandInsertLineBreakMenuCaption = 'InsertLineBreak';
  sdxRichEditCommandInsertMergeFieldDescription = 'Insert Merge Field.';
  sdxRichEditCommandInsertMergeFieldMenuCaption = 'Insert Merge Field';
  sdxRichEditCommandInsertMultilevelListDescription =
    'Start a multilevel list.';
  sdxRichEditCommandInsertMultilevelListMenuCaption = 'Multilevel list';
  sdxRichEditCommandInsertNonBreakingSpaceDescription =
    'InsertNonBreakingSpace';
  sdxRichEditCommandInsertNonBreakingSpaceMenuCaption =
    'InsertNonBreakingSpace';
  sdxRichEditCommandInsertPageBreakDescription =
    'Start the next page at the current position.';
  sdxRichEditCommandInsertPageBreakMenuCaption = 'Page';
  sdxRichEditCommandInsertPageCountFieldDescription =
    'Insert total page count into the document.';
  sdxRichEditCommandInsertPageCountFieldMenuCaption = 'Page Count';
  sdxRichEditCommandInsertPageNumberFieldDescription =
    'Insert page numbers into the document.';
  sdxRichEditCommandInsertPageNumberFieldMenuCaption = 'Page Number';
  sdxRichEditCommandInsertParagraphDescription = 'InsertParagraph';
  sdxRichEditCommandInsertParagraphMenuCaption = 'InsertParagraph';
  sdxRichEditCommandInsertPictureDescription =
    'Insert inline picture from a file.';
  sdxRichEditCommandInsertPictureMenuCaption = 'Inline Picture';
  sdxRichEditCommandInsertRegisteredTrademarkSymbolDescription =
    'InsertRegisteredTrademark Symbol';
  sdxRichEditCommandInsertRegisteredTrademarkSymbolMenuCaption =
    'InsertRegisteredTrademark Symbol';
  sdxRichEditCommandInsertSectionBreakContinuousDescription =
    'Insert a section break and start the new section on the same page.';
  sdxRichEditCommandInsertSectionBreakContinuousMenuCaption =
    'Section (Continuous)';
  sdxRichEditCommandInsertSectionBreakEvenPageDescription =
    'Insert a section break and start the new section on the next even-numbered page.';
  sdxRichEditCommandInsertSectionBreakEvenPageMenuCaption =
    'Section (Even Page)';
  sdxRichEditCommandInsertSectionBreakNextPageDescription =
    'Insert a section break and start the new section on the next page.';
  sdxRichEditCommandInsertSectionBreakNextPageMenuCaption =
    'Section (Next Page)';
  sdxRichEditCommandInsertSectionBreakOddPageDescription =
    'Insert a section break and start the new section on the next odd-numbered page.';
  sdxRichEditCommandInsertSectionBreakOddPageMenuCaption = 'Section (Odd Page)';
  sdxRichEditCommandInsertSimpleListDescription = 'Start a numbered list.';
  sdxRichEditCommandInsertSimpleListMenuCaption = 'Numbering';
  sdxRichEditCommandInsertSymbolDescription =
    'Insert symbols that are not on your keyboard, such as copyright symbols, trademark symbols, paragraph marks and Unicode characters.';
  sdxRichEditCommandInsertSymbolMenuCaption = 'Symbol';
  sdxRichEditCommandInsertTabDescription = 'InsertTab';
  sdxRichEditCommandInsertTableCellsDescription = 'Insert Cells';
  sdxRichEditCommandInsertTableCellsMenuCaption = 'Insert Cells';
  sdxRichEditCommandInsertTableColumnToTheLeftDescription =
    'Add a new column directly to the left of the selected column.';
  sdxRichEditCommandInsertTableColumnToTheLeftMenuCaption = 'Insert Left';
  sdxRichEditCommandInsertTableColumnToTheRightDescription =
    'Add a new column directly to the right of the selected column.';
  sdxRichEditCommandInsertTableColumnToTheRightMenuCaption = 'Insert Right';
  sdxRichEditCommandInsertTableDescription =
    'Insert a table into the document.';
  sdxRichEditCommandInsertTableElementMenuCaption = 'Insert';
  sdxRichEditCommandInsertTableMenuCaption = 'Table';
  sdxRichEditCommandInsertTableOfContentsDescription =
    'Add the Table of Contents to the document.'#13#10'Once you have added a Table of Contents, click the Add Text button to add entries to the table.';
  sdxRichEditCommandInsertTableOfContentsMenuCaption = 'Table of Contents';
  sdxRichEditCommandInsertTableOfEquationsDescription =
    'Insert a Table of Equations into the document.'#13#10'A Table of Equations includes a list of all the equations in the document.';
  sdxRichEditCommandInsertTableOfEquationsMenuCaption = 'Table of Equations';
  sdxRichEditCommandInsertTableOfFiguresDescription =
    'Insert a Table of Figures into the document.'#13#10'A Table of Figures includes a list of all the figures in the document.';
  sdxRichEditCommandInsertTableOfFiguresMenuCaption = 'Table of Figures';
  sdxRichEditCommandInsertTableOfFiguresPlaceholderDescription =
    'Insert a Table of Figures into the document.'#13#10'A Table of Figures includes a list of all of the figures, tables or equations in the document.';
  sdxRichEditCommandInsertTableOfFiguresPlaceholderMenuCaption =
    'Insert Table of Figures';
  sdxRichEditCommandInsertTableOfTablesDescription =
    'Insert a Table of Tables into the document.'#13#10'A Table of Tables includes a list of all the tables in the document.';
  sdxRichEditCommandInsertTableOfTablesMenuCaption = 'Table of Tables';
  sdxRichEditCommandInsertTableRowAboveDescription =
    'Add a new row directly above the selected row.';
  sdxRichEditCommandInsertTableRowAboveMenuCaption = 'Insert Above';
  sdxRichEditCommandInsertTableRowBelowDescription =
    'Add a new row directly below the selected row.';
  sdxRichEditCommandInsertTableRowBelowMenuCaption = 'Insert Below';
  sdxRichEditCommandInsertTableCaptionDescription = 'Add a table caption.';
  sdxRichEditCommandInsertTableCaptionMenuCaption = 'Tables Caption';
  sdxRichEditCommandInsertTabMenuCaption = 'InsertTab';
  sdxRichEditCommandInsertTabToParagraphDescription =
    'InsertTabToParagraphDescription';
  sdxRichEditCommandInsertTabToParagraphMenuCaption = 'InsertTabToParagraph';
  sdxRichEditCommandInsertTextBoxDescription =
    'Insert a text box into the document.';
  sdxRichEditCommandInsertTextBoxMenuCaption = 'Text Box';
  sdxRichEditCommandInsertTextDescription = 'InsertText';
  sdxRichEditCommandInsertTextMenuCaption = 'InsertText';
  sdxRichEditCommandInsertTrademarkSymbolDescription = 'InsertTrademarkSymbol';
  sdxRichEditCommandInsertTrademarkSymbolMenuCaption = 'InsertTrademarkSymbol';
  sdxRichEditCommandLoadDocumentDescription = 'Open a document.';
  sdxRichEditCommandLoadDocumentMenuCaption = 'Open';
  sdxRichEditCommandMailMergeSaveDocumentAsCommandMenuCaption = 'Mail Merge';
  sdxRichEditCommandMakeTextLowerCaseDescription =
    'Change all the selected text to lowercase.';
  sdxRichEditCommandMakeTextLowerCaseMenuCaption = 'lowercase';
  sdxRichEditCommandMakeTextUpperCaseDescription =
    'Change all the selected text to UPPERCASE.';
  sdxRichEditCommandMakeTextUpperCaseMenuCaption = 'UPPERCASE';
  sdxRichEditCommandMergeTableCellsDescription =
    'Merge the selected cells into one cell.';
  sdxRichEditCommandMergeTableCellsMenuCaption = 'Merge Cells';
  sdxRichEditCommandMoveBackwardDescription = 'MoveBackward';
  sdxRichEditCommandMoveBackwardMenuCaption = 'MoveBackward';
  sdxRichEditCommandMoveForwardDescription = 'MoveForward';
  sdxRichEditCommandMoveForwardMenuCaption = 'MoveForward';
  sdxRichEditCommandMoveLineDownDescription = 'MoveLineDown';
  sdxRichEditCommandMoveLineDownMenuCaption = 'MoveLineDown';
  sdxRichEditCommandMoveLineUpDescription = 'MoveLineUp';
  sdxRichEditCommandMoveLineUpMenuCaption = 'MoveLineUp';
  sdxRichEditCommandMoveNextPageDescription = 'MoveNextPage';
  sdxRichEditCommandMoveNextPageMenuCaption = 'MoveNextPage';
  sdxRichEditCommandMoveNextParagraphDescription = 'MoveNextParagraph';
  sdxRichEditCommandMoveNextParagraphMenuCaption = 'MoveNextParagraph';
  sdxRichEditCommandMoveNextWordDescription = 'MoveNextWord';
  sdxRichEditCommandMoveNextWordMenuCaption = 'MoveNextWord';
  sdxRichEditCommandMovePreviousPageDescription = 'MovePreviousPage';
  sdxRichEditCommandMovePreviousPageMenuCaption = 'MovePreviousPage';
  sdxRichEditCommandMovePreviousParagraphDescription = 'MovePreviousParagraph';
  sdxRichEditCommandMovePreviousParagraphMenuCaption = 'MovePreviousParagraph';
  sdxRichEditCommandMovePreviousWordDescription = 'MovePreviousWord';
  sdxRichEditCommandMovePreviousWordMenuCaption = 'MovePreviousWord';
  sdxRichEditCommandMoveScreenDownDescription = 'MoveScreenDown';
  sdxRichEditCommandMoveScreenDownMenuCaption = 'MoveScreenDown';
  sdxRichEditCommandMoveScreenUpDescription = 'MoveScreenUp';
  sdxRichEditCommandMoveScreenUpMenuCaption = 'MoveScreenUp';
  sdxRichEditCommandMoveToBeginOfDocumentDescription =
    'MoveToBeginBeginOfDocument';
  sdxRichEditCommandMoveToBeginOfDocumentMenuCaption =
    'MoveToBeginBeginOfDocument';
  sdxRichEditCommandMoveToEndOfDocumentDescription = 'MoveToEndOfDocument';
  sdxRichEditCommandMoveToEndOfDocumentMenuCaption = 'MoveToEndOfDocument';
  sdxRichEditCommandMoveToEndOfLineDescription = 'MoveToEndOfLine';
  sdxRichEditCommandMoveToEndOfLineMenuCaption = 'MoveToEndOfLine';
  sdxRichEditCommandMoveToStartOfLineDescription = 'MoveToBeginOfLine';
  sdxRichEditCommandMoveToStartOfLineMenuCaption = 'MoveToBeginOfLine';
  sdxRichEditCommandNewEmptyDocumentDescription = 'Create a new document.';
  sdxRichEditCommandNewEmptyDocumentMenuCaption = 'New';
  sdxRichEditCommandOpenHyperlinkAtCaretPositionDescription =
    'OpenHyperlinkAtCaretPositionDescription';
  sdxRichEditCommandOpenHyperlinkAtCaretPositionMenuCaption =
    'OpenHyperlinkAtCaretPosition';
  sdxRichEditCommandOpenHyperlinkDescription = 'Open hyperlink';
  sdxRichEditCommandOpenHyperlinkMenuCaption = 'Open hyperlink';
  sdxRichEditCommandOvertypeTextDescription = 'OvertypeText';
  sdxRichEditCommandOvertypeTextMenuCaption = 'OvertypeText';
  sdxRichEditCommandParagraphAlignmentCenterDescription = 'Center text.';
  sdxRichEditCommandParagraphAlignmentCenterMenuCaption = '&Center';
  sdxRichEditCommandParagraphAlignmentJustifyDescription =
    'Align text to both left and right margins, adding extra space between words as necessary.'#13#10#13#10'This creates a clean look along the left and right side of the page.';
  sdxRichEditCommandParagraphAlignmentJustifyMenuCaption = '&Justify';
  sdxRichEditCommandParagraphAlignmentLeftDescription =
    'Align text to the left.';
  sdxRichEditCommandParagraphAlignmentLeftMenuCaption = 'Align Text &Left';
  sdxRichEditCommandParagraphAlignmentRightDescription =
    'Align text to the right.';
  sdxRichEditCommandParagraphAlignmentRightMenuCaption = 'Align Text &Right';
  sdxRichEditCommandPasteDescription = 'Paste the contents of the Clipboard.';
  sdxRichEditCommandPasteFilesDescription =
    'Inserts the contents of the Clipboard as an embedded file.';
  sdxRichEditCommandPasteFilesMenuCaption = 'Files';
  sdxRichEditCommandPasteHtmlTextDescription =
    'Inserts the contents of the Clipboard as HTML format.';
  sdxRichEditCommandPasteHtmlTextMenuCaption = 'HTML Format';
  sdxRichEditCommandPasteImageDescription =
    'Inserts the contents of the Clipboard as picture.';
  sdxRichEditCommandPasteImageMenuCaption = 'Picture';
  sdxRichEditCommandPasteMenuCaption = '&Paste';
  sdxRichEditCommandPasteMetafileImageDescription =
    'Inserts the contents of the Clipboard as metafile.';
  sdxRichEditCommandPasteMetafileImageMenuCaption = 'Metafile';
  sdxRichEditCommandPastePlainTextDescription =
    'Inserts the contents of the Clipboard as text without any formatting.';
  sdxRichEditCommandPastePlainTextMenuCaption = 'Unformatted text';
  sdxRichEditCommandPasteRtfTextDescription =
    'Inserts the contents of the Clipboard as text with font and table formatting.';
  sdxRichEditCommandPasteRtfTextMenuCaption = 'Formatted text (RTF)';
  sdxRichEditCommandPlaceCaretToPhysicalPointDescription =
    'PlaceCaretToPhysicalPoint';
  sdxRichEditCommandPlaceCaretToPhysicalPointMenuCaption =
    'PlaceCaretToPhysicalPoint';
  sdxRichEditCommandPrintDescription =
    'Select a printer, number of copies, and other printing options before printing.';
  sdxRichEditCommandPrintMenuCaption = '&Print';
  sdxRichEditCommandPrintPreviewDescription = 'Preview pages before printing.';
  sdxRichEditCommandPrintPreviewMenuCaption = 'Print Pre&view';
  sdxRichEditCommandProtectDocumentDescription =
    'Help restrict people from editing the document by specifying a password.';
  sdxRichEditCommandProtectDocumentMenuCaption = 'Protect Document';
  sdxRichEditCommandRedoDescription = 'Redo the last operation.';
  sdxRichEditCommandRedoMenuCaption = '&Redo';
  sdxRichEditCommandRemoveHyperlinkDescription = 'Remove hyperlink';
  sdxRichEditCommandRemoveHyperlinkMenuCaption = 'Remove hyperlink';
  sdxRichEditCommandReplaceAllBackwardDescription =
    'ReplaceAllBackwardDescription';
  sdxRichEditCommandReplaceAllBackwardMenuCaption = 'ReplaceAllBackward';
  sdxRichEditCommandReplaceAllForwardDescription =
    'ReplaceAllForwardDescription';
  sdxRichEditCommandReplaceBackwardDescription = 'ReplaceBackward';
  sdxRichEditCommandReplaceBackwardMenuCaption = 'ReplaceBackward';
  sdxRichEditCommandReplaceDescription = 'Replace text in the document.';
  sdxRichEditCommandReplaceForwardDescription = 'ReplaceForward';
  sdxRichEditCommandReplaceForwardMenuCaption = 'ReplaceForward';
  sdxRichEditCommandReplaceMenuCaption = 'Replace';
  sdxRichEditCommandResetTableCellsBordersDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandResetTableCellsBordersMenuCaption = '&No Border';
  sdxRichEditCommandSaveDocumentAsDescription =
    'Open the Save As dialog box to select a file format and save the document to a new location.';
  sdxRichEditCommandSaveDocumentAsMenuCaption = 'Save As';
  sdxRichEditCommandSaveDocumentDescription = 'Save the document.';
  sdxRichEditCommandSaveDocumentMenuCaption = 'Save';
  sdxRichEditCommandSelectAllDescription = 'Select entire document content.';
  sdxRichEditCommandSelectAllMenuCaption = 'Select &All';
  sdxRichEditCommandSelectBookmarkDescription = 'Select Bookmark';
  sdxRichEditCommandSelectBookmarkMenuCaption = 'Select Bookmark';
  sdxRichEditCommandSelectFieldNextToCaretDescription =
    'SelectFieldNextToCaretDescription';
  sdxRichEditCommandSelectFieldNextToCaretMenuCaption =
    'SelectFieldNextToCaret';
  sdxRichEditCommandSelectFieldPrevToCaretDescription =
    'SelectFieldPrevToCaretDescription';
  sdxRichEditCommandSelectFieldPrevToCaretMenuCaption =
    'SelectFieldPrevToCaret';
  sdxRichEditCommandSelectTableCellDescription = 'Select Cell';
  sdxRichEditCommandSelectTableCellMenuCaption = 'Select Cell';
  sdxRichEditCommandSelectTableColumnsDescription = 'Select Column';
  sdxRichEditCommandSelectTableColumnsMenuCaption = 'Select Column';
  sdxRichEditCommandSelectTableRowDescription = 'Select Row';
  sdxRichEditCommandSelectTableRowMenuCaption = 'Select Row';
  sdxRichEditCommandSetDoubleParagraphSpacingDescription = '';
  sdxRichEditCommandSetDoubleParagraphSpacingMenuCaption = '2.0';
  sdxRichEditCommandSetFloatingObjectBehindTextWrapTypeDescription = '';
  sdxRichEditCommandSetFloatingObjectBehindTextWrapTypeMenuCaption =
    'Behind Text';
  sdxRichEditCommandSetFloatingObjectBottomCenterAlignmentDescription =
    'Position in Bottom Center with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectBottomCenterAlignmentMenuCaption =
    'Bottom Center';
  sdxRichEditCommandSetFloatingObjectBottomLeftAlignmentDescription =
    'Position in Bottom Left with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectBottomLeftAlignmentMenuCaption =
    'Bottom Left';
  sdxRichEditCommandSetFloatingObjectBottomRightAlignmentDescription =
    'Position in Bottom Right with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectBottomRightAlignmentMenuCaption =
    'Bottom Right';
  sdxRichEditCommandSetFloatingObjectInFrontOfTextWrapTypeDescription = '';
  sdxRichEditCommandSetFloatingObjectInFrontOfTextWrapTypeMenuCaption =
    'In Front of Text';
  sdxRichEditCommandSetFloatingObjectMiddleCenterAlignmentDescription =
    'Position in Middle Center with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectMiddleCenterAlignmentMenuCaption =
    'Middle Center';
  sdxRichEditCommandSetFloatingObjectMiddleLeftAlignmentDescription =
    'Position in Middle Left with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectMiddleLeftAlignmentMenuCaption =
    'Middle Left';
  sdxRichEditCommandSetFloatingObjectMiddleRightAlignmentDescription =
    'Position in Middle Right with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectMiddleRightAlignmentMenuCaption =
    'Middle Right';
  sdxRichEditCommandSetFloatingObjectSquareTextWrapTypeDescription = '';
  sdxRichEditCommandSetFloatingObjectSquareTextWrapTypeMenuCaption = 'Square';
  sdxRichEditCommandSetFloatingObjectThroughTextWrapTypeDescription = '';
  sdxRichEditCommandSetFloatingObjectThroughTextWrapTypeMenuCaption = 'Through';
  sdxRichEditCommandSetFloatingObjectTightTextWrapTypeDescription = '';
  sdxRichEditCommandSetFloatingObjectTightTextWrapTypeMenuCaption = 'Tight';
  sdxRichEditCommandSetFloatingObjectTopAndBottomTextWrapTypeDescription = '';
  sdxRichEditCommandSetFloatingObjectTopAndBottomTextWrapTypeMenuCaption =
    'Top and Bottom';
  sdxRichEditCommandSetFloatingObjectTopCenterAlignmentDescription =
    'Position in Top Center with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectTopCenterAlignmentMenuCaption =
    'Top Center';
  sdxRichEditCommandSetFloatingObjectTopLeftAlignmentDescription =
    'Position in Top Left with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectTopLeftAlignmentMenuCaption = 'Top Left';
  sdxRichEditCommandSetFloatingObjectTopRightAlignmentDescription =
    'Position in Top Right with Square Text Wrapping';
  sdxRichEditCommandSetFloatingObjectTopRightAlignmentMenuCaption = 'Top Right';
  sdxRichEditCommandSetLandscapePageOrientationDescription = '';
  sdxRichEditCommandSetLandscapePageOrientationMenuCaption = 'Landscape';
  sdxRichEditCommandSetParagraphBodyTextLevelDescription =
    'Do Not Show in Table of Contents';
  sdxRichEditCommandSetParagraphBodyTextLevelMenuCaption =
    'Do Not Show in Table of Contents';
  sdxRichEditCommandSetParagraphHeadingLevelDescription = 'Level %d';
  sdxRichEditCommandSetParagraphHeadingLevelMenuCaption = 'Level %d';
  sdxRichEditCommandSetPortraitPageOrientationDescription = '';
  sdxRichEditCommandSetPortraitPageOrientationMenuCaption = 'Portrait';
  sdxRichEditCommandSetSectionColumnsDescription =
    'Split text into two or more columns.';
  sdxRichEditCommandSetSectionLineNumberingContinuousDescription = 'Continuous';
  sdxRichEditCommandSetSectionLineNumberingContinuousMenuCaption = 'Continuous';
  sdxRichEditCommandSetSectionLineNumberingNoneDescription = 'No line numbers.';
  sdxRichEditCommandSetSectionLineNumberingNoneMenuCaption = 'None';
  sdxRichEditCommandSetSectionLineNumberingRestartNewPageDescription =
    'Restart Each Page';
  sdxRichEditCommandSetSectionLineNumberingRestartNewPageMenuCaption =
    'Restart Each Page';
  sdxRichEditCommandSetSectionLineNumberingRestartNewSectionDescription =
    'Restart Each Section';
  sdxRichEditCommandSetSectionLineNumberingRestartNewSectionMenuCaption =
    'Restart Each Section';
  sdxRichEditCommandSetSectionOneColumnDescription = 'One column.';
  sdxRichEditCommandSetSectionOneColumnMenuCaption = 'One';
  sdxRichEditCommandSetSectionThreeColumnsDescription = 'Three columns.';
  sdxRichEditCommandSetSectionThreeColumnsMenuCaption = 'Three';
  sdxRichEditCommandSetSectionTwoColumnsDescription = 'Two columns.';
  sdxRichEditCommandSetSectionTwoColumnsMenuCaption = 'Two';
  sdxRichEditCommandSetSesquialteralParagraphSpacingDescription = '';
  sdxRichEditCommandSetSesquialteralParagraphSpacingMenuCaption = '1.5';
  sdxRichEditCommandSetSingleParagraphSpacingDescription = '';
  sdxRichEditCommandSetSingleParagraphSpacingMenuCaption = '1.0';
  sdxRichEditCommandShowAllFieldCodesDescription =
    'View the document markup with dynamic elements displaying their rich-text codes.';
  sdxRichEditCommandShowAllFieldCodesMenuCaption = 'Show All Field Codes';
  sdxRichEditCommandShowAllFieldResultsDescription =
    'View the document content with dynamic elements displaying real data.';
  sdxRichEditCommandShowAllFieldResultsMenuCaption = 'Show All Field Results';
  sdxRichEditCommandShowColumnsSetupFormDescription =
    'Show the Columns dialog box to customize column widths.';
  sdxRichEditCommandShowColumnsSetupFormMenuCaption = 'More &Columns...';
  sdxRichEditCommandShowEditStyleFormDescription =
    'Show the Edit Style dialog box.';
  sdxRichEditCommandShowEditStyleFormMenuCaption = 'Modify Style...';
  sdxRichEditCommandShowFloatingObjectLayoutOptionsFormDescription =
    'Show the Layout dialog box.';
  sdxRichEditCommandShowFloatingObjectLayoutOptionsFormMenuCaption =
    'More Layout Options...';
  sdxRichEditCommandShowFontFormDescription = 'Show the Font dialog box.';
  sdxRichEditCommandShowFontFormMenuCaption = '&Font...';
  sdxRichEditCommandShowMergeDatabaseRecordsFormDescription =
    'Merge to New Document';
  sdxRichEditCommandShowMergeDatabaseRecordsFormMenuCaption =
    'Merge to New Document';
  sdxRichEditCommandShowHyperlinkFormDescription =
    'Create a link to a Web page, a picture, an e-mail address, or a program.';
  sdxRichEditCommandShowHyperlinkFormMenuCaption = 'Hyperlink';
  sdxRichEditCommandShowInsertMergeFieldFormDescription =
    'Add a field from a list of recipients or from a data table to the document.';
  sdxRichEditCommandShowInsertMergeFieldFormMenuCaption = 'Insert Merge Field';
  sdxRichEditCommandShowLineNumberingFormDescription =
    'Line Numbering Options...';
  sdxRichEditCommandShowLineNumberingFormMenuCaption =
    '&Line Numbering Options...';
  sdxRichEditCommandShowNumberingListDescription =
    'Show the Numbered List dialog box.';
  sdxRichEditCommandShowNumberingListMenuCaption = '&Bullets and Numbering...';
  sdxRichEditCommandShowPageMarginsSetupFormDescription = '';
  sdxRichEditCommandShowPageMarginsSetupFormMenuCaption = 'Custom M&argins...';
  sdxRichEditCommandShowPagePaperSetupFormDescription = '';
  sdxRichEditCommandShowPagePaperSetupFormMenuCaption = 'More P&aper Sizes...';
  sdxRichEditCommandShowPageSetupFormDescription =
    'Show the Page Setup dialog box.';
  sdxRichEditCommandShowPageSetupFormMenuCaption = 'Page Setup';
  sdxRichEditCommandShowParagraphFormDescription =
    'Show the Paragraph dialog box.';
  sdxRichEditCommandShowParagraphFormMenuCaption = 'P&aragraph...';
  sdxRichEditCommandShowRangeEditingPermissionsFormDescription =
    'Grant user permissions to edit the selected part of the document.';
  sdxRichEditCommandShowRangeEditingPermissionsFormMenuCaption =
    'Range Editing Permissions';
  sdxRichEditCommandShowSpellingDialogMenuCaption = 'Spelling';
  sdxRichEditCommandShowSpellingDialogDescription =
    'Check the spelling of text in the document.';
  sdxRichEditCommandShowSymbolDescription = 'Show the Symbol dialog box.';
  sdxRichEditCommandShowSymbolMenuCaption = 'Symbol';
  sdxRichEditCommandShowTablePropertiesFormDescription =
    'Show the Table Properties dialog box to change advanced table properties, such as indentation and text wrapping options.';
  sdxRichEditCommandShowTablePropertiesFormDescriptionMenuItemMenuCaption =
    'Show the Table Properties dialog box.';
  sdxRichEditCommandShowTablePropertiesFormMenuItemMenuCaption =
    'Table Properties...';
  sdxRichEditCommandShowTabsFormDescription = 'Tabs';
  sdxRichEditCommandShowTabsFormMenuCaption = 'Tabs...';
  sdxRichEditCommandSplitTableCellsDescription =
    'Split the selected cells into multiple new cells.';
  sdxRichEditCommandSplitTableCellsMenuCaption = 'Split Cells';
  sdxRichEditCommandSplitTableCellsMenuItemMenuCaption = 'Split Cells...';
  sdxRichEditCommandSplitTableDescription =
    'Split the table into two tables.'#13#10#13#10'The selected row will become the first row of the new table.';
  sdxRichEditCommandSplitTableMenuCaption = 'Split Table';
  sdxRichEditCommandSwitchToDraftViewDescription =
    'View the document as a draft to quickly edit the text.'#13#10#13#10'Certain elements of the document such as headers and footers will not be visible in this view.';
  sdxRichEditCommandSwitchToDraftViewMenuCaption = 'Draft View';
  sdxRichEditCommandSwitchToPrintLayoutViewDescription =
    'View the document as it will appear on the printed page.';
  sdxRichEditCommandSwitchToPrintLayoutViewMenuCaption = 'Print Layout';
  sdxRichEditCommandSwitchToSimpleViewDescription =
    'View the document as a simple memo.'#13#10#13#10'This view ignores the page layout to draw attention to text editing.';
  sdxRichEditCommandSwitchToSimpleViewMenuCaption = 'Simple View';
  sdxRichEditCommandTabKeyDescription = 'TabKey';
  sdxRichEditCommandTabKeyMenuCaption = 'TabKey';
  sdxRichEditCommandToggleDifferentFirstPageDescription =
    'Specify a unique header and footer for the first page of the document.';
  sdxRichEditCommandToggleDifferentFirstPageMenuCaption =
    'Different First Page';
  sdxRichEditCommandToggleDifferentOddAndEvenPagesDescription =
    'Specify that odd-numbered pages should have a different header and footer from even-numbered pages.';
  sdxRichEditCommandToggleDifferentOddAndEvenPagesMenuCaption =
    'Different Odd && Even Pages';
  sdxRichEditCommandToggleFieldCodesDescription = 'Toggle Field Codes';
  sdxRichEditCommandToggleFieldCodesMenuCaption = 'Toggle Field Codes';
  sdxRichEditCommandToggleFontBoldDescription = 'Make the selected text bold.';
  sdxRichEditCommandToggleFontBoldMenuCaption = '&Bold';
  sdxRichEditCommandToggleFontDoubleStrikeoutDescription =
    'Double strikethrough';
  sdxRichEditCommandToggleFontDoubleStrikeoutMenuCaption =
    'Double Strikethrough';
  sdxRichEditCommandToggleFontDoubleUnderlineDescription = 'Double underline';
  sdxRichEditCommandToggleFontDoubleUnderlineMenuCaption = 'Double Underline';
  sdxRichEditCommandToggleFontItalicDescription =
    'Italicize the selected text.';
  sdxRichEditCommandToggleFontItalicMenuCaption = '&Italic';
  sdxRichEditCommandToggleFontStrikeoutDescription =
    'Draw a line through the middle of the selected text.';
  sdxRichEditCommandToggleFontStrikeoutMenuCaption = 'Strikethrough';
  sdxRichEditCommandToggleFontUnderlineDescription =
    'Underline the selected text.';
  sdxRichEditCommandToggleFontUnderlineMenuCaption = '&Underline';
  sdxRichEditCommandToggleHeaderFooterLinkToPreviousDescription =
    'Link to the previous section so that the header and footer in the current section contain the same content as in the previous section.';
  sdxRichEditCommandToggleHeaderFooterLinkToPreviousMenuCaption =
    'Link to Previous';
  sdxRichEditCommandToggleOvertypeDescription = 'Overtype';
  sdxRichEditCommandToggleOvertypeMenuCaption = 'Overtype';
  sdxRichEditCommandToggleShowHorizontalRulerDescription =
    'View the horizontal ruler, used to measure and line up objects in the document.';
  sdxRichEditCommandToggleShowHorizontalRulerMenuCaption = 'Horizontal Ruler';
  sdxRichEditCommandToggleShowTableGridLinesDescription =
    'Show or hide the gridlines within the table.';
  sdxRichEditCommandToggleShowTableGridLinesMenuCaption = 'View &Gridlines';
  sdxRichEditCommandToggleShowVerticalRulerDescription =
    'View the vertical ruler, used to measure and line up objects in the document.';
  sdxRichEditCommandToggleShowVerticalRulerMenuCaption = 'Vertical Ruler';
  sdxRichEditCommandToggleTableAutoFitContentsDescription =
    'Auto-Fit Table to the contents.';
  sdxRichEditCommandToggleTableAutoFitContentsMenuCaption = 'AutoFit Contents';
  sdxRichEditCommandToggleTableAutoFitPlaceholderDescription =
    'Automatically resize the column widths based on the text in them.'#13#10#13#10'You can set the table width based on the window size or convert it back to use fixed column widths.';
  sdxRichEditCommandToggleTableAutoFitPlaceholderMenuCaption = 'AutoFit';
  sdxRichEditCommandToggleTableAutoFitWindowDescription =
    'Auto-Fit Table to the window.';
  sdxRichEditCommandToggleTableAutoFitWindowMenuCaption = 'AutoFit Window';
  sdxRichEditCommandToggleTableCellsAllBordersDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsAllBordersMenuCaption = '&All Borders';
  sdxRichEditCommandToggleTableCellsBottomBorderDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsBottomBorderMenuCaption = '&Bottom Border';
  sdxRichEditCommandToggleTableCellsBottomCenterAlignmentDescription =
    'Center text and align it to the bottom of the cell.';
  sdxRichEditCommandToggleTableCellsBottomCenterAlignmentMenuCaption =
    'Align Bottom Center';
  sdxRichEditCommandToggleTableCellsBottomLeftAlignmentDescription =
    'Align text to the bottom left corner of the cell.';
  sdxRichEditCommandToggleTableCellsBottomLeftAlignmentMenuCaption =
    'Align Bottom Left';
  sdxRichEditCommandToggleTableCellsBottomRightAlignmentDescription =
    'Align text to the bottom right corner of the cell.';
  sdxRichEditCommandToggleTableCellsBottomRightAlignmentMenuCaption =
    'Align Bottom Right';
  sdxRichEditCommandToggleTableCellsInsideBorderDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsInsideBorderMenuCaption = '&Inside Borders';
  sdxRichEditCommandToggleTableCellsInsideHorizontalBorderDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsInsideHorizontalBorderMenuCaption =
    'Inside &Horizontal Border';
  sdxRichEditCommandToggleTableCellsInsideVerticalBorderDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsInsideVerticalBorderMenuCaption =
    'Inside &Vertical Border';
  sdxRichEditCommandToggleTableCellsLeftBorderDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsLeftBorderMenuCaption = '&Left Border';
  sdxRichEditCommandToggleTableCellsMiddleCenterAlignmentDescription =
    'Center text horizontally and vertically within the cell.';
  sdxRichEditCommandToggleTableCellsMiddleCenterAlignmentMenuCaption =
    'Align Center';
  sdxRichEditCommandToggleTableCellsMiddleLeftAlignmentDescription =
    'Center text vertically and align it to the left side of the cell.';
  sdxRichEditCommandToggleTableCellsMiddleLeftAlignmentMenuCaption =
    'Align Center Left';
  sdxRichEditCommandToggleTableCellsMiddleRightAlignmentDescription =
    'Center text vertically and align it to the right side of the cell.';
  sdxRichEditCommandToggleTableCellsMiddleRightAlignmentMenuCaption =
    'Align Center Right';
  sdxRichEditCommandToggleTableCellsOutsideBorderDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsOutsideBorderMenuCaption =
    'Out&side Borders';
  sdxRichEditCommandToggleTableCellsRightBorderDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsRightBorderMenuCaption = '&Right Border';
  sdxRichEditCommandToggleTableCellsTopBorderDescription =
    'Customize the borders of the selected cells.';
  sdxRichEditCommandToggleTableCellsTopBorderMenuCaption = 'To&p Border';
  sdxRichEditCommandToggleTableCellsTopCenterAlignmentDescription =
    'Center text and align it to the top of the cell.';
  sdxRichEditCommandToggleTableCellsTopCenterAlignmentMenuCaption =
    'Align Top Center';
  sdxRichEditCommandToggleTableCellsTopLeftAlignmentDescription =
    'Align text to the top left corner of the cell.';
  sdxRichEditCommandToggleTableCellsTopLeftAlignmentMenuCaption =
    'Align Top Left';
  sdxRichEditCommandToggleTableCellsTopRightAlignmentDescription =
    'Align text to the top right corner of the cell.';
  sdxRichEditCommandToggleTableCellsTopRightAlignmentMenuCaption =
    'Align Top Right';
  sdxRichEditCommandToggleTableFixedColumnWidthDescription =
    'Set table size to a fixed width.';
  sdxRichEditCommandToggleTableFixedColumnWidthMenuCaption =
    'Fixed Column Width';
  sdxRichEditCommandToggleTextCaseDescription = 'tOGGLE cASE.';
  sdxRichEditCommandToggleTextCaseMenuCaption = 'tOGGLE cASE';
  sdxRichEditCommandToggleViewMergedDataDescription =
    'Replaces the merge fields in your document with actual data from your recipient list so you can see what it looks like.';
  sdxRichEditCommandToggleViewMergedDataMenuCaption = 'View Merged Data';
  sdxRichEditCommandToggleWhitespaceDescription =
    'Show paragraph marks and other hidden formatting symbols.';
  sdxRichEditCommandToggleWhitespaceMenuCaption = 'Show/Hide �';
  sdxRichEditCommandUndoDescription = 'Undo the last operation.';
  sdxRichEditCommandUndoMenuCaption = '&Undo';
  sdxRichEditCommandUnprotectDocumentDescription =
    'Enable users to edit the document.';
  sdxRichEditCommandUnprotectDocumentMenuCaption = 'Unprotect Document';
  sdxRichEditCommandUpdateFieldDescription = 'Update Field';
  sdxRichEditCommandUpdateFieldMenuCaption = 'Update Field';
  sdxRichEditCommandUpdateFieldsDescription = 'Update Fields';
  sdxRichEditCommandUpdateFieldsMenuCaption = 'Update Fields';
  sdxRichEditCommandUpdateTableOfContentsDescription =
    'Update the Table of Contents so that all the entries refer to the correct page number.';
  sdxRichEditCommandUpdateTableOfContentsMenuCaption = 'Update Table';
  sdxRichEditCommandUpdateTableOfFiguresDescription =
    'Update the Table of Figures to include all of the entries in the document.';
  sdxRichEditCommandUpdateTableOfFiguresMenuCaption = 'Update Table';
  sdxRichEditCommandZoomDescription = 'Zoom';
  sdxRichEditCommandZoomInDescription =
    'Zoom in to get a close-up view of the document.';
  sdxRichEditCommandZoomInMenuCaption = 'Zoom In';
  sdxRichEditCommandZoomMenuCaption = 'Zoom';
  sdxRichEditCommandZoomOutDescription =
    'Zoom out to see more of the page at a reduced size.';
  sdxRichEditCommandZoomOutMenuCaption = 'Zoom Out';

  sdxRichEditCommandCaptionPrefixTable = 'Table';
  sdxRichEditCommandCaptionPrefixEquation = 'Equation';
  sdxRichEditCommandCaptionPrefixFigure = 'Figure';

  sdxRichEditInsertHyperlinkTitle = 'Insert Hyperlink';
  sdxRichEditEditHyperlinkTitle = 'Edit Hyperlink';

  sdxRichEditCommandPaperSizeGalleryCaption = 'Size';
  sdxRichEditCommandPaperSizeGalleryUnitsInchesCaption = 'in';
  sdxRichEditCommandPaperSizeGalleryUnitsMillimetersCaption = 'mm';
  sdxRichEditCommandPageMarginsGalleryCaption = 'Margins';
  sdxRichEditCommandPageMarginsGalleryNormalMarginsCaption = 'Normal';
  sdxRichEditCommandPageMarginsGalleryNarrowMarginsCaption = 'Narrow';
  sdxRichEditCommandPageMarginsGalleryModerateMarginsCaption = 'Moderate';
  sdxRichEditCommandPageMarginsGalleryWideMarginsCaption = 'Wide';
  sdxRichEditCommandPageMarginsGalleryTopPartCaption = 'Top';
  sdxRichEditCommandPageMarginsGalleryBottomPartCaption = 'Bottom';
  sdxRichEditCommandPageMarginsGalleryLeftPartCaption = 'Left';
  sdxRichEditCommandPageMarginsGalleryRightPartCaption = 'Right';
  sdxRichEditCommandQuickStylesGalleryCaption = 'Quick Styles';
  sdxRichEditCommandTableStylesGalleryCaption = 'Table Styles';

implementation

uses
  dxCore, cxClasses,
  dxRichEdit.Strs;

procedure AddRichEditTableCommandsStringNames
  (AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxRichEditCommandInsertTableCaptionMenuCaption',
    @sdxRichEditCommandInsertTableCaptionMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableCaptionDescription',
    @sdxRichEditCommandInsertTableCaptionDescription);
  AProduct.Add('sdxRichEditCommandUpdateTableOfContentsMenuCaption',
    @sdxRichEditCommandUpdateTableOfContentsMenuCaption);
  AProduct.Add('sdxRichEditCommandUpdateTableOfContentsDescription',
    @sdxRichEditCommandUpdateTableOfContentsDescription);
  AProduct.Add('sdxRichEditCommandUpdateTableOfFiguresMenuCaption',
    @sdxRichEditCommandUpdateTableOfFiguresMenuCaption);
  AProduct.Add('sdxRichEditCommandUpdateTableOfFiguresDescription',
    @sdxRichEditCommandUpdateTableOfFiguresDescription);
  AProduct.Add('sdxRichEditCommandChangeTableCellsContentAlignmentDescription',
    @sdxRichEditCommandChangeTableCellsContentAlignmentDescription);
  AProduct.Add('sdxRichEditCommandChangeTableCellsContentAlignmentMenuCaption',
    @sdxRichEditCommandChangeTableCellsContentAlignmentMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteTableCellsDescription',
    @sdxRichEditCommandDeleteTableCellsDescription);
  AProduct.Add('sdxRichEditCommandDeleteTableCellsMenuCaption',
    @sdxRichEditCommandDeleteTableCellsMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteTableCellsMenuItem',
    @sdxRichEditCommandDeleteTableCellsMenuItem);
  AProduct.Add('sdxRichEditCommandDeleteTableColumnsDescription',
    @sdxRichEditCommandDeleteTableColumnsDescription);
  AProduct.Add('sdxRichEditCommandDeleteTableColumnsMenuCaption',
    @sdxRichEditCommandDeleteTableColumnsMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteTableDescription',
    @sdxRichEditCommandDeleteTableDescription);
  AProduct.Add('sdxRichEditCommandDeleteTableMenuCaption',
    @sdxRichEditCommandDeleteTableMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteTableRowsDescription',
    @sdxRichEditCommandDeleteTableRowsDescription);
  AProduct.Add('sdxRichEditCommandDeleteTableRowsMenuCaption',
    @sdxRichEditCommandDeleteTableRowsMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableCellsDescription',
    @sdxRichEditCommandInsertTableCellsDescription);
  AProduct.Add('sdxRichEditCommandInsertTableCellsMenuCaption',
    @sdxRichEditCommandInsertTableCellsMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableDescription',
    @sdxRichEditCommandInsertTableDescription);
  AProduct.Add('sdxRichEditCommandInsertTableElementMenuCaption',
    @sdxRichEditCommandInsertTableElementMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableMenuCaption',
    @sdxRichEditCommandInsertTableMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableRowAboveDescription',
    @sdxRichEditCommandInsertTableRowAboveDescription);
  AProduct.Add('sdxRichEditCommandInsertTableRowAboveMenuCaption',
    @sdxRichEditCommandInsertTableRowAboveMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableRowBelowDescription',
    @sdxRichEditCommandInsertTableRowBelowDescription);
  AProduct.Add('sdxRichEditCommandInsertTableRowBelowMenuCaption',
    @sdxRichEditCommandInsertTableRowBelowMenuCaption);
  AProduct.Add('sdxRichEditCommandResetTableCellsBordersDescription',
    @sdxRichEditCommandResetTableCellsBordersDescription);
  AProduct.Add('sdxRichEditCommandResetTableCellsBordersMenuCaption',
    @sdxRichEditCommandResetTableCellsBordersMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleShowTableGridLinesDescription',
    @sdxRichEditCommandToggleShowTableGridLinesDescription);
  AProduct.Add('sdxRichEditCommandToggleShowTableGridLinesMenuCaption',
    @sdxRichEditCommandToggleShowTableGridLinesMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableAutoFitContentsDescription',
    @sdxRichEditCommandToggleTableAutoFitContentsDescription);
  AProduct.Add('sdxRichEditCommandToggleTableAutoFitContentsMenuCaption',
    @sdxRichEditCommandToggleTableAutoFitContentsMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableAutoFitPlaceholderDescription',
    @sdxRichEditCommandToggleTableAutoFitPlaceholderDescription);
  AProduct.Add('sdxRichEditCommandToggleTableAutoFitPlaceholderMenuCaption',
    @sdxRichEditCommandToggleTableAutoFitPlaceholderMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableAutoFitWindowDescription',
    @sdxRichEditCommandToggleTableAutoFitWindowDescription);
  AProduct.Add('sdxRichEditCommandToggleTableAutoFitWindowMenuCaption',
    @sdxRichEditCommandToggleTableAutoFitWindowMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsAllBordersDescription',
    @sdxRichEditCommandToggleTableCellsAllBordersDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsAllBordersMenuCaption',
    @sdxRichEditCommandToggleTableCellsAllBordersMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsBottomBorderDescription',
    @sdxRichEditCommandToggleTableCellsBottomBorderDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsBottomBorderMenuCaption',
    @sdxRichEditCommandToggleTableCellsBottomBorderMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsBottomCenterAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsBottomCenterAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsBottomCenterAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsBottomCenterAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsBottomLeftAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsBottomLeftAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsBottomLeftAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsBottomLeftAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsBottomRightAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsBottomRightAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsBottomRightAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsBottomRightAlignmentMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsInsideBorderDescription',
    @sdxRichEditCommandToggleTableCellsInsideBorderDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsInsideBorderMenuCaption',
    @sdxRichEditCommandToggleTableCellsInsideBorderMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsInsideHorizontalBorderDescription',
    @sdxRichEditCommandToggleTableCellsInsideHorizontalBorderDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsInsideHorizontalBorderMenuCaption',
    @sdxRichEditCommandToggleTableCellsInsideHorizontalBorderMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsInsideVerticalBorderDescription',
    @sdxRichEditCommandToggleTableCellsInsideVerticalBorderDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsInsideVerticalBorderMenuCaption',
    @sdxRichEditCommandToggleTableCellsInsideVerticalBorderMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsLeftBorderDescription',
    @sdxRichEditCommandToggleTableCellsLeftBorderDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsLeftBorderMenuCaption',
    @sdxRichEditCommandToggleTableCellsLeftBorderMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsMiddleCenterAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsMiddleCenterAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsMiddleCenterAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsMiddleCenterAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsMiddleLeftAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsMiddleLeftAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsMiddleLeftAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsMiddleLeftAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsMiddleRightAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsMiddleRightAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsMiddleRightAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsMiddleRightAlignmentMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsOutsideBorderDescription',
    @sdxRichEditCommandToggleTableCellsOutsideBorderDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsOutsideBorderMenuCaption',
    @sdxRichEditCommandToggleTableCellsOutsideBorderMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsRightBorderDescription',
    @sdxRichEditCommandToggleTableCellsRightBorderDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsRightBorderMenuCaption',
    @sdxRichEditCommandToggleTableCellsRightBorderMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsTopBorderDescription',
    @sdxRichEditCommandToggleTableCellsTopBorderDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsTopBorderMenuCaption',
    @sdxRichEditCommandToggleTableCellsTopBorderMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsTopCenterAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsTopCenterAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandToggleTableCellsTopCenterAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsTopCenterAlignmentMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsTopLeftAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsTopLeftAlignmentDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsTopLeftAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsTopLeftAlignmentMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableCellsTopRightAlignmentDescription',
    @sdxRichEditCommandToggleTableCellsTopRightAlignmentDescription);
  AProduct.Add('sdxRichEditCommandToggleTableCellsTopRightAlignmentMenuCaption',
    @sdxRichEditCommandToggleTableCellsTopRightAlignmentMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTableFixedColumnWidthDescription',
    @sdxRichEditCommandToggleTableFixedColumnWidthDescription);
  AProduct.Add('sdxRichEditCommandToggleTableFixedColumnWidthMenuCaption',
    @sdxRichEditCommandToggleTableFixedColumnWidthMenuCaption);
  AProduct.Add('sdxRichEditCommandMergeTableCellsMenuCaption',
    @sdxRichEditCommandMergeTableCellsMenuCaption);
  AProduct.Add('sdxRichEditCommandMergeTableCellsDescription',
    @sdxRichEditCommandMergeTableCellsDescription);
  AProduct.Add('sdxRichEditCommandSplitTableMenuCaption',
    @sdxRichEditCommandSplitTableMenuCaption);
  AProduct.Add('sdxRichEditCommandSplitTableDescription',
    @sdxRichEditCommandSplitTableDescription);
  AProduct.Add('sdxRichEditCommandSplitTableCellsMenuCaption',
    @sdxRichEditCommandSplitTableCellsMenuCaption);
  AProduct.Add('sdxRichEditCommandSplitTableCellsMenuItemMenuCaption',
    @sdxRichEditCommandSplitTableCellsMenuItemMenuCaption);
  AProduct.Add('sdxRichEditCommandSplitTableCellsDescription',
    @sdxRichEditCommandSplitTableCellsDescription);
  AProduct.Add('sdxRichEditCommandInsertTableColumnToTheLeftMenuCaption',
    @sdxRichEditCommandInsertTableColumnToTheLeftMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableColumnToTheLeftDescription',
    @sdxRichEditCommandInsertTableColumnToTheLeftDescription);
  AProduct.Add('sdxRichEditCommandInsertTableColumnToTheRightMenuCaption',
    @sdxRichEditCommandInsertTableColumnToTheRightMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableColumnToTheRightDescription',
    @sdxRichEditCommandInsertTableColumnToTheRightDescription);
  AProduct.Add('sdxRichEditCommandSelectTableColumnsMenuCaption',
    @sdxRichEditCommandSelectTableColumnsMenuCaption);
  AProduct.Add('sdxRichEditCommandSelectTableColumnsDescription',
    @sdxRichEditCommandSelectTableColumnsDescription);
  AProduct.Add('sdxRichEditCommandSelectTableCellMenuCaption',
    @sdxRichEditCommandSelectTableCellMenuCaption);
  AProduct.Add('sdxRichEditCommandSelectTableCellDescription',
    @sdxRichEditCommandSelectTableCellDescription);
  AProduct.Add('sdxRichEditCommandSelectTableRowMenuCaption',
    @sdxRichEditCommandSelectTableRowMenuCaption);
  AProduct.Add('sdxRichEditCommandSelectTableRowDescription',
    @sdxRichEditCommandSelectTableRowDescription);
  AProduct.Add('sdxRichEditCommandShowTablePropertiesFormDescription',
    @sdxRichEditCommandShowTablePropertiesFormDescription);
  AProduct.Add('sdxRichEditCommandShowTablePropertiesFormMenuItemMenuCaption',
    @sdxRichEditCommandShowTablePropertiesFormMenuItemMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandShowTablePropertiesFormDescriptionMenuItemMenuCaption',
    @sdxRichEditCommandShowTablePropertiesFormDescriptionMenuItemMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableOfContentsMenuCaption',
    @sdxRichEditCommandInsertTableOfContentsMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableOfContentsDescription',
    @sdxRichEditCommandInsertTableOfContentsDescription);
  AProduct.Add('sdxRichEditCommandInsertTableOfEquationsMenuCaption',
    @sdxRichEditCommandInsertTableOfEquationsMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableOfEquationsDescription',
    @sdxRichEditCommandInsertTableOfEquationsDescription);
  AProduct.Add('sdxRichEditCommandInsertTableOfFiguresMenuCaption',
    @sdxRichEditCommandInsertTableOfFiguresMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableOfFiguresDescription',
    @sdxRichEditCommandInsertTableOfFiguresDescription);
  AProduct.Add('sdxRichEditCommandInsertTableOfTablesMenuCaption',
    @sdxRichEditCommandInsertTableOfTablesMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableOfTablesDescription',
    @sdxRichEditCommandInsertTableOfTablesDescription);
  AProduct.Add('sdxRichEditCommandInsertTableOfFiguresPlaceholderMenuCaption',
    @sdxRichEditCommandInsertTableOfFiguresPlaceholderMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTableOfFiguresPlaceholderDescription',
    @sdxRichEditCommandInsertTableOfFiguresPlaceholderDescription);
end;

procedure AddRichEditParagraphCommandsStringNames
  (AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxRichEditCommandChangeParagraphFirstLineIndentDescription',
    @sdxRichEditCommandChangeParagraphFirstLineIndentDescription);
  AProduct.Add('sdxRichEditCommandChangeParagraphFirstLineIndentMenuCaption',
    @sdxRichEditCommandChangeParagraphFirstLineIndentMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeParagraphLeftIndentDescription',
    @sdxRichEditCommandChangeParagraphLeftIndentDescription);
  AProduct.Add('sdxRichEditCommandChangeParagraphLeftIndentMenuCaption',
    @sdxRichEditCommandChangeParagraphLeftIndentMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeParagraphRightIndentDescription',
    @sdxRichEditCommandChangeParagraphRightIndentDescription);
  AProduct.Add('sdxRichEditCommandChangeParagraphRightIndentMenuCaption',
    @sdxRichEditCommandChangeParagraphRightIndentMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeParagraphStyleDescription',
    @sdxRichEditCommandChangeParagraphStyleDescription);
  AProduct.Add('sdxRichEditCommandChangeParagraphStyleMenuCaption',
    @sdxRichEditCommandChangeParagraphStyleMenuCaption);
  AProduct.Add('sdxRichEditCommandDecrementNumerationFromParagraphDescription',
    @sdxRichEditCommandDecrementNumerationFromParagraphDescription);
  AProduct.Add('sdxRichEditCommandDecrementNumerationFromParagraphMenuCaption',
    @sdxRichEditCommandDecrementNumerationFromParagraphMenuCaption);
  AProduct.Add('sdxRichEditCommandDecrementParagraphLeftIndentDescription',
    @sdxRichEditCommandDecrementParagraphLeftIndentDescription);
  AProduct.Add('sdxRichEditCommandDecrementParagraphLeftIndentMenuCaption',
    @sdxRichEditCommandDecrementParagraphLeftIndentMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteNumerationFromParagraphDescription',
    @sdxRichEditCommandDeleteNumerationFromParagraphDescription);
  AProduct.Add('sdxRichEditCommandDeleteNumerationFromParagraphMenuCaption',
    @sdxRichEditCommandDeleteNumerationFromParagraphMenuCaption);
  AProduct.Add('sdxRichEditCommandIncrementNumerationFromParagraphDescription',
    @sdxRichEditCommandIncrementNumerationFromParagraphDescription);
  AProduct.Add('sdxRichEditCommandIncrementNumerationFromParagraphMenuCaption',
    @sdxRichEditCommandIncrementNumerationFromParagraphMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertParagraphDescription',
    @sdxRichEditCommandInsertParagraphDescription);
  AProduct.Add('sdxRichEditCommandInsertParagraphMenuCaption',
    @sdxRichEditCommandInsertParagraphMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTabToParagraphDescription',
    @sdxRichEditCommandInsertTabToParagraphDescription);
  AProduct.Add('sdxRichEditCommandInsertTabToParagraphMenuCaption',
    @sdxRichEditCommandInsertTabToParagraphMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveNextParagraphDescription',
    @sdxRichEditCommandMoveNextParagraphDescription);
  AProduct.Add('sdxRichEditCommandMoveNextParagraphMenuCaption',
    @sdxRichEditCommandMoveNextParagraphMenuCaption);
  AProduct.Add('sdxRichEditCommandMovePreviousParagraphDescription',
    @sdxRichEditCommandMovePreviousParagraphDescription);
  AProduct.Add('sdxRichEditCommandMovePreviousParagraphMenuCaption',
    @sdxRichEditCommandMovePreviousParagraphMenuCaption);
  AProduct.Add('sdxRichEditCommandParagraphAlignmentCenterDescription',
    @sdxRichEditCommandParagraphAlignmentCenterDescription);
  AProduct.Add('sdxRichEditCommandParagraphAlignmentCenterMenuCaption',
    @sdxRichEditCommandParagraphAlignmentCenterMenuCaption);
  AProduct.Add('sdxRichEditCommandParagraphAlignmentJustifyDescription',
    @sdxRichEditCommandParagraphAlignmentJustifyDescription);
  AProduct.Add('sdxRichEditCommandParagraphAlignmentJustifyMenuCaption',
    @sdxRichEditCommandParagraphAlignmentJustifyMenuCaption);
  AProduct.Add('sdxRichEditCommandParagraphAlignmentLeftDescription',
    @sdxRichEditCommandParagraphAlignmentLeftDescription);
  AProduct.Add('sdxRichEditCommandParagraphAlignmentLeftMenuCaption',
    @sdxRichEditCommandParagraphAlignmentLeftMenuCaption);
  AProduct.Add('sdxRichEditCommandParagraphAlignmentRightDescription',
    @sdxRichEditCommandParagraphAlignmentRightDescription);
  AProduct.Add('sdxRichEditCommandParagraphAlignmentRightMenuCaption',
    @sdxRichEditCommandParagraphAlignmentRightMenuCaption);
  AProduct.Add('sdxRichEditCommandSetDoubleParagraphSpacingDescription',
    @sdxRichEditCommandSetDoubleParagraphSpacingDescription);
  AProduct.Add('sdxRichEditCommandSetDoubleParagraphSpacingMenuCaption',
    @sdxRichEditCommandSetDoubleParagraphSpacingMenuCaption);
  AProduct.Add('sdxRichEditCommandSetSesquialteralParagraphSpacingDescription',
    @sdxRichEditCommandSetSesquialteralParagraphSpacingDescription);
  AProduct.Add('sdxRichEditCommandSetSesquialteralParagraphSpacingMenuCaption',
    @sdxRichEditCommandSetSesquialteralParagraphSpacingMenuCaption);
  AProduct.Add('sdxRichEditCommandSetSingleParagraphSpacingDescription',
    @sdxRichEditCommandSetSingleParagraphSpacingDescription);
  AProduct.Add('sdxRichEditCommandSetSingleParagraphSpacingMenuCaption',
    @sdxRichEditCommandSetSingleParagraphSpacingMenuCaption);
  AProduct.Add('sdxRichEditCommandShowParagraphFormDescription',
    @sdxRichEditCommandShowParagraphFormDescription);
  AProduct.Add('sdxRichEditCommandShowParagraphFormMenuCaption',
    @sdxRichEditCommandShowParagraphFormMenuCaption);
  AProduct.Add('sdxRichEditCommandIncrementParagraphLeftIndentDescription',
    @sdxRichEditCommandIncrementParagraphLeftIndentDescription);
  AProduct.Add('sdxRichEditCommandIncrementParagraphLeftIndentMenuCaption',
    @sdxRichEditCommandIncrementParagraphLeftIndentMenuCaption);
  AProduct.Add('sdxRichEditCommandIncrementParagraphOutlineLevelMenuCaption',
    @sdxRichEditCommandIncrementParagraphOutlineLevelMenuCaption);
  AProduct.Add('sdxRichEditCommandIncrementParagraphOutlineLevelDescription',
    @sdxRichEditCommandIncrementParagraphOutlineLevelDescription);
  AProduct.Add('sdxRichEditCommandDecrementParagraphOutlineLevelMenuCaption',
    @sdxRichEditCommandDecrementParagraphOutlineLevelMenuCaption);
  AProduct.Add('sdxRichEditCommandDecrementParagraphOutlineLevelDescription',
    @sdxRichEditCommandDecrementParagraphOutlineLevelDescription);
  AProduct.Add('sdxRichEditCommandSetParagraphBodyTextLevelMenuCaption',
    @sdxRichEditCommandSetParagraphBodyTextLevelMenuCaption);
  AProduct.Add('sdxRichEditCommandSetParagraphBodyTextLevelDescription',
    @sdxRichEditCommandSetParagraphBodyTextLevelDescription);
  AProduct.Add('sdxRichEditCommandSetParagraphHeadingLevelMenuCaption',
    @sdxRichEditCommandSetParagraphHeadingLevelMenuCaption);
  AProduct.Add('sdxRichEditCommandSetParagraphHeadingLevelDescription',
    @sdxRichEditCommandSetParagraphHeadingLevelDescription);
  AProduct.Add('sdxRichEditCommandAddParagraphsToTableOfContentsMenuCaption',
    @sdxRichEditCommandAddParagraphsToTableOfContentsMenuCaption);
  AProduct.Add('sdxRichEditCommandAddParagraphsToTableOfContentsDescription',
    @sdxRichEditCommandAddParagraphsToTableOfContentsDescription);
end;

procedure AddRichEditCommandsStringNames(AProduct: TdxProductResourceStrings);
begin
  AddRichEditParagraphCommandsStringNames(AProduct);
  AddRichEditTableCommandsStringNames(AProduct);
  AProduct.Add('sdxRichEditCommandAddWordToDictionaryDescription',
    @sdxRichEditCommandAddWordToDictionaryDescription);
  AProduct.Add('sdxRichEditCommandAddWordToDictionaryMenuCaption',
    @sdxRichEditCommandAddWordToDictionaryMenuCaption);
  AProduct.Add('sdxRichEditCommandAutoCorrectPlaceholderDescription',
    @sdxRichEditCommandAutoCorrectPlaceholderDescription);
  AProduct.Add('sdxRichEditCommandAutoCorrectPlaceholderMenuCaption',
    @sdxRichEditCommandAutoCorrectPlaceholderMenuCaption);
  AProduct.Add('sdxRichEditCommandBackSpaceKeyDescription',
    @sdxRichEditCommandBackSpaceKeyDescription);
  AProduct.Add('sdxRichEditCommandBackSpaceKeyMenuCaption',
    @sdxRichEditCommandBackSpaceKeyMenuCaption);
  AProduct.Add('sdxRichEditCommandBookmarkDescription',
    @sdxRichEditCommandBookmarkDescription);
  AProduct.Add('sdxRichEditCommandBookmarkMenuCaption',
    @sdxRichEditCommandBookmarkMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeColumnSizeDescription',
    @sdxRichEditCommandChangeColumnSizeDescription);
  AProduct.Add('sdxRichEditCommandChangeColumnSizeMenuCaption',
    @sdxRichEditCommandChangeColumnSizeMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeFontColorDescription',
    @sdxRichEditCommandChangeFontColorDescription);
  AProduct.Add('sdxRichEditCommandChangeFontColorMenuCaption',
    @sdxRichEditCommandChangeFontColorMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeFontNameDescription',
    @sdxRichEditCommandChangeFontNameDescription);
  AProduct.Add('sdxRichEditCommandChangeFontNameMenuCaption',
    @sdxRichEditCommandChangeFontNameMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeFontSizeDescription',
    @sdxRichEditCommandChangeFontSizeDescription);
  AProduct.Add('sdxRichEditCommandChangeFontSizeMenuCaption',
    @sdxRichEditCommandChangeFontSizeMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeIndentDescription',
    @sdxRichEditCommandChangeIndentDescription);
  AProduct.Add('sdxRichEditCommandChangeIndentMenuCaption',
    @sdxRichEditCommandChangeIndentMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeMistakenWordDescription',
    @sdxRichEditCommandChangeMistakenWordDescription);
  AProduct.Add('sdxRichEditCommandChangeMistakenWordMenuCaption',
    @sdxRichEditCommandChangeMistakenWordMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeSectionPagePaperKindDescription',
    @sdxRichEditCommandChangeSectionPagePaperKindDescription);
  AProduct.Add('sdxRichEditCommandChangeSectionPagePaperKindMenuCaption',
    @sdxRichEditCommandChangeSectionPagePaperKindMenuCaption);
  AProduct.Add('sdxRichEditCommandClearFormattingMenuCaption',
    @sdxRichEditCommandClearFormattingMenuCaption);
  AProduct.Add('sdxRichEditCommandClearUndoDescription',
    @sdxRichEditCommandClearUndoDescription);
  AProduct.Add('sdxRichEditCommandClearUndoMenuCaption',
    @sdxRichEditCommandClearUndoMenuCaption);
  AProduct.Add('sdxRichEditCommandClosePageHeaderFooterDescription',
    @sdxRichEditCommandClosePageHeaderFooterDescription);
  AProduct.Add('sdxRichEditCommandClosePageHeaderFooterMenuCaption',
    @sdxRichEditCommandClosePageHeaderFooterMenuCaption);
  AProduct.Add('sdxRichEditCommandCopySelectionDescription',
    @sdxRichEditCommandCopySelectionDescription);
  AProduct.Add('sdxRichEditCommandCopySelectionMenuCaption',
    @sdxRichEditCommandCopySelectionMenuCaption);
  AProduct.Add('sdxRichEditCommandCreateBookmarkDescription',
    @sdxRichEditCommandCreateBookmarkDescription);
  AProduct.Add('sdxRichEditCommandCreateBookmarkMenuCaption',
    @sdxRichEditCommandCreateBookmarkMenuCaption);
  AProduct.Add('sdxRichEditCommandCreateFieldDescription',
    @sdxRichEditCommandCreateFieldDescription);
  AProduct.Add('sdxRichEditCommandCreateFieldMenuCaption',
    @sdxRichEditCommandCreateFieldMenuCaption);
  AProduct.Add('sdxRichEditCommandCreateHyperlinkDescription',
    @sdxRichEditCommandCreateHyperlinkDescription);
  AProduct.Add('sdxRichEditCommandCreateHyperlinkMenuCaption',
    @sdxRichEditCommandCreateHyperlinkMenuCaption);
  AProduct.Add('sdxRichEditCommandCutSelectionDescription',
    @sdxRichEditCommandCutSelectionDescription);
  AProduct.Add('sdxRichEditCommandCutSelectionMenuCaption',
    @sdxRichEditCommandCutSelectionMenuCaption);
  AProduct.Add('sdxRichEditCommandDecreaseFontSizeDescription',
    @sdxRichEditCommandDecreaseFontSizeDescription);
  AProduct.Add('sdxRichEditCommandDecreaseFontSizeMenuCaption',
    @sdxRichEditCommandDecreaseFontSizeMenuCaption);
  AProduct.Add('sdxRichEditCommandDecrementFontSizeDescription',
    @sdxRichEditCommandDecrementFontSizeDescription);
  AProduct.Add('sdxRichEditCommandDecrementFontSizeMenuCaption',
    @sdxRichEditCommandDecrementFontSizeMenuCaption);
  AProduct.Add('sdxRichEditCommandDecrementIndentDescription',
    @sdxRichEditCommandDecrementIndentDescription);
  AProduct.Add('sdxRichEditCommandDecrementIndentMenuCaption',
    @sdxRichEditCommandDecrementIndentMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteBackCoreDescription',
    @sdxRichEditCommandDeleteBackCoreDescription);
  AProduct.Add('sdxRichEditCommandDeleteBackCoreMenuCaption',
    @sdxRichEditCommandDeleteBackCoreMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteBackDescription',
    @sdxRichEditCommandDeleteBackDescription);
  AProduct.Add('sdxRichEditCommandDeleteBackMenuCaption',
    @sdxRichEditCommandDeleteBackMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteBookmarkDescription',
    @sdxRichEditCommandDeleteBookmarkDescription);
  AProduct.Add('sdxRichEditCommandDeleteBookmarkMenuCaption',
    @sdxRichEditCommandDeleteBookmarkMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteDescription',
    @sdxRichEditCommandDeleteDescription);
  AProduct.Add('sdxRichEditCommandDeleteMenuCaption',
    @sdxRichEditCommandDeleteMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteRepeatedWordDescription',
    @sdxRichEditCommandDeleteRepeatedWordDescription);
  AProduct.Add('sdxRichEditCommandDeleteRepeatedWordMenuCaption',
    @sdxRichEditCommandDeleteRepeatedWordMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteWordBackCoreDescription',
    @sdxRichEditCommandDeleteWordBackCoreDescription);
  AProduct.Add('sdxRichEditCommandDeleteWordBackCoreMenuCaption',
    @sdxRichEditCommandDeleteWordBackCoreMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteWordBackDescription',
    @sdxRichEditCommandDeleteWordBackDescription);
  AProduct.Add('sdxRichEditCommandDeleteWordBackMenuCaption',
    @sdxRichEditCommandDeleteWordBackMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteWordCoreDescription',
    @sdxRichEditCommandDeleteWordCoreDescription);
  AProduct.Add('sdxRichEditCommandDeleteWordCoreMenuCaption',
    @sdxRichEditCommandDeleteWordCoreMenuCaption);
  AProduct.Add('sdxRichEditCommandDeleteWordDescription',
    @sdxRichEditCommandDeleteWordDescription);
  AProduct.Add('sdxRichEditCommandDeleteWordMenuCaption',
    @sdxRichEditCommandDeleteWordMenuCaption);
  AProduct.Add('sdxRichEditCommandDeselectAllDescription',
    @sdxRichEditCommandDeselectAllDescription);
  AProduct.Add('sdxRichEditCommandDeselectAllMenuCaption',
    @sdxRichEditCommandDeselectAllMenuCaption);
  AProduct.Add('sdxRichEditCommandEditHyperlinkDescription',
    @sdxRichEditCommandEditHyperlinkDescription);
  AProduct.Add('sdxRichEditCommandEditHyperlinkMenuCaption',
    @sdxRichEditCommandEditHyperlinkMenuCaption);
  AProduct.Add('sdxRichEditCommandEditPageFooterDescription',
    @sdxRichEditCommandEditPageFooterDescription);
  AProduct.Add('sdxRichEditCommandEditPageFooterMenuCaption',
    @sdxRichEditCommandEditPageFooterMenuCaption);
  AProduct.Add('sdxRichEditCommandEditPageHeaderDescription',
    @sdxRichEditCommandEditPageHeaderDescription);
  AProduct.Add('sdxRichEditCommandEditPageHeaderMenuCaption',
    @sdxRichEditCommandEditPageHeaderMenuCaption);
  AProduct.Add('sdxRichEditCommandEditTOCMenuCaption',
    @sdxRichEditCommandEditTOCMenuCaption);
  AProduct.Add('sdxRichEditCommandEncryptDocumentDescription',
    @sdxRichEditCommandEncryptDocumentDescription);
  AProduct.Add('sdxRichEditCommandEncryptDocumentMenuCaption',
    @sdxRichEditCommandEncryptDocumentMenuCaption);
  AProduct.Add('sdxRichEditCommandEnsureCaretVisibleHorizontallyDescription',
    @sdxRichEditCommandEnsureCaretVisibleHorizontallyDescription);
  AProduct.Add('sdxRichEditCommandEnsureCaretVisibleHorizontallyMenuCaption',
    @sdxRichEditCommandEnsureCaretVisibleHorizontallyMenuCaption);
  AProduct.Add('sdxRichEditCommandEnsureCaretVisibleVerticallyDescription',
    @sdxRichEditCommandEnsureCaretVisibleVerticallyDescription);
  AProduct.Add('sdxRichEditCommandEnsureCaretVisibleVerticallyMenuCaption',
    @sdxRichEditCommandEnsureCaretVisibleVerticallyMenuCaption);
  AProduct.Add('sdxRichEditCommandEnterKeyDescription',
    @sdxRichEditCommandEnterKeyDescription);
  AProduct.Add('sdxRichEditCommandEnterKeyMenuCaption',
    @sdxRichEditCommandEnterKeyMenuCaption);
  AProduct.Add('sdxRichEditCommandFindAndSelectBackwardDescription',
    @sdxRichEditCommandFindAndSelectBackwardDescription);
  AProduct.Add('sdxRichEditCommandFindAndSelectBackwardMenuCaption',
    @sdxRichEditCommandFindAndSelectBackwardMenuCaption);
  AProduct.Add('sdxRichEditCommandFindAndSelectForwardDescription',
    @sdxRichEditCommandFindAndSelectForwardDescription);
  AProduct.Add('sdxRichEditCommandFindAndSelectForwardMenuCaption',
    @sdxRichEditCommandFindAndSelectForwardMenuCaption);
  AProduct.Add('sdxRichEditCommandFindDescription',
    @sdxRichEditCommandFindDescription);
  AProduct.Add('sdxRichEditCommandFindMenuCaption',
    @sdxRichEditCommandFindMenuCaption);
  AProduct.Add('sdxRichEditCommandFindNextDescription',
    @sdxRichEditCommandFindNextDescription);
  AProduct.Add('sdxRichEditCommandFindNextMenuCaption',
    @sdxRichEditCommandFindNextMenuCaption);
  AProduct.Add('sdxRichEditCommandFindPrevDescription',
    @sdxRichEditCommandFindPrevDescription);
  AProduct.Add('sdxRichEditCommandFindPrevMenuCaption',
    @sdxRichEditCommandFindPrevMenuCaption);
  AProduct.Add('sdxRichEditCommandFontSubscriptDescription',
    @sdxRichEditCommandFontSubscriptDescription);
  AProduct.Add('sdxRichEditCommandFontSubscriptMenuCaption',
    @sdxRichEditCommandFontSubscriptMenuCaption);
  AProduct.Add('sdxRichEditCommandFontSuperscriptDescription',
    @sdxRichEditCommandFontSuperscriptDescription);
  AProduct.Add('sdxRichEditCommandFontSuperscriptMenuCaption',
    @sdxRichEditCommandFontSuperscriptMenuCaption);
  AProduct.Add('sdxRichEditCommandGoToNextHeaderFooterDescription',
    @sdxRichEditCommandGoToNextHeaderFooterDescription);
  AProduct.Add('sdxRichEditCommandGoToNextHeaderFooterMenuCaption',
    @sdxRichEditCommandGoToNextHeaderFooterMenuCaption);
  AProduct.Add('sdxRichEditCommandGoToPageFooterDescription',
    @sdxRichEditCommandGoToPageFooterDescription);
  AProduct.Add('sdxRichEditCommandGoToPageFooterMenuCaption',
    @sdxRichEditCommandGoToPageFooterMenuCaption);
  AProduct.Add('sdxRichEditCommandGoToPageHeaderDescription',
    @sdxRichEditCommandGoToPageHeaderDescription);
  AProduct.Add('sdxRichEditCommandGoToPageHeaderMenuCaption',
    @sdxRichEditCommandGoToPageHeaderMenuCaption);
  AProduct.Add('sdxRichEditCommandGoToPreviousHeaderFooterDescription',
    @sdxRichEditCommandGoToPreviousHeaderFooterDescription);
  AProduct.Add('sdxRichEditCommandGoToPreviousHeaderFooterMenuCaption',
    @sdxRichEditCommandGoToPreviousHeaderFooterMenuCaption);
  AProduct.Add('sdxRichEditCommandHighlightTextDescription',
    @sdxRichEditCommandHighlightTextDescription);
  AProduct.Add('sdxRichEditCommandHighlightTextMenuCaption',
    @sdxRichEditCommandHighlightTextMenuCaption);
  AProduct.Add('sdxRichEditCommandHyperlinkDescription',
    @sdxRichEditCommandHyperlinkDescription);
  AProduct.Add('sdxRichEditCommandHyperlinkMenuCaption',
    @sdxRichEditCommandHyperlinkMenuCaption);
  AProduct.Add('sdxRichEditCommandIgnoreAllMistakenWordsDescription',
    @sdxRichEditCommandIgnoreAllMistakenWordsDescription);
  AProduct.Add('sdxRichEditCommandIgnoreAllMistakenWordsMenuCaption',
    @sdxRichEditCommandIgnoreAllMistakenWordsMenuCaption);
  AProduct.Add('sdxRichEditCommandIgnoreMistakenWordDescription',
    @sdxRichEditCommandIgnoreMistakenWordDescription);
  AProduct.Add('sdxRichEditCommandIgnoreMistakenWordMenuCaption',
    @sdxRichEditCommandIgnoreMistakenWordMenuCaption);
  AProduct.Add('sdxRichEditCommandIncreaseFontSizeDescription',
    @sdxRichEditCommandIncreaseFontSizeDescription);
  AProduct.Add('sdxRichEditCommandIncreaseFontSizeMenuCaption',
    @sdxRichEditCommandIncreaseFontSizeMenuCaption);
  AProduct.Add('sdxRichEditCommandIncrementFontSizeDescription',
    @sdxRichEditCommandIncrementFontSizeDescription);
  AProduct.Add('sdxRichEditCommandIncrementFontSizeMenuCaption',
    @sdxRichEditCommandIncrementFontSizeMenuCaption);
  AProduct.Add('sdxRichEditCommandIncrementIndentDescription',
    @sdxRichEditCommandIncrementIndentDescription);
  AProduct.Add('sdxRichEditCommandIncrementIndentMenuCaption',
    @sdxRichEditCommandIncrementIndentMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertBreakDescription',
    @sdxRichEditCommandInsertBreakDescription);
  AProduct.Add('sdxRichEditCommandInsertBreakMenuCaption',
    @sdxRichEditCommandInsertBreakMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertBulletListDescription',
    @sdxRichEditCommandInsertBulletListDescription);
  AProduct.Add('sdxRichEditCommandInsertBulletListMenuCaption',
    @sdxRichEditCommandInsertBulletListMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertColumnBreakDescription',
    @sdxRichEditCommandInsertColumnBreakDescription);
  AProduct.Add('sdxRichEditCommandInsertColumnBreakMenuCaption',
    @sdxRichEditCommandInsertColumnBreakMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertCopyrightSymbolDescription',
    @sdxRichEditCommandInsertCopyrightSymbolDescription);
  AProduct.Add('sdxRichEditCommandInsertCopyrightSymbolMenuCaption',
    @sdxRichEditCommandInsertCopyrightSymbolMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertEllipsisDescription',
    @sdxRichEditCommandInsertEllipsisDescription);
  AProduct.Add('sdxRichEditCommandInsertEllipsisMenuCaption',
    @sdxRichEditCommandInsertEllipsisMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertEmDashDescription',
    @sdxRichEditCommandInsertEmDashDescription);
  AProduct.Add('sdxRichEditCommandInsertEmDashMenuCaption',
    @sdxRichEditCommandInsertEmDashMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertEnDashDescription',
    @sdxRichEditCommandInsertEnDashDescription);
  AProduct.Add('sdxRichEditCommandInsertEnDashMenuCaption',
    @sdxRichEditCommandInsertEnDashMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertFieldDescription',
    @sdxRichEditCommandInsertFieldDescription);
  AProduct.Add('sdxRichEditCommandInsertFieldMenuCaption',
    @sdxRichEditCommandInsertFieldMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertHyperlinkDescription',
    @sdxRichEditCommandInsertHyperlinkDescription);
  AProduct.Add('sdxRichEditCommandInsertHyperlinkMenuCaption',
    @sdxRichEditCommandInsertHyperlinkMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertLineBreakDescription',
    @sdxRichEditCommandInsertLineBreakDescription);
  AProduct.Add('sdxRichEditCommandInsertLineBreakMenuCaption',
    @sdxRichEditCommandInsertLineBreakMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertMergeFieldDescription',
    @sdxRichEditCommandInsertMergeFieldDescription);
  AProduct.Add('sdxRichEditCommandInsertMergeFieldMenuCaption',
    @sdxRichEditCommandInsertMergeFieldMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertMultilevelListDescription',
    @sdxRichEditCommandInsertMultilevelListDescription);
  AProduct.Add('sdxRichEditCommandInsertMultilevelListMenuCaption',
    @sdxRichEditCommandInsertMultilevelListMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertNonBreakingSpaceDescription',
    @sdxRichEditCommandInsertNonBreakingSpaceDescription);
  AProduct.Add('sdxRichEditCommandInsertNonBreakingSpaceMenuCaption',
    @sdxRichEditCommandInsertNonBreakingSpaceMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertPageBreakDescription',
    @sdxRichEditCommandInsertPageBreakDescription);
  AProduct.Add('sdxRichEditCommandInsertPageBreakMenuCaption',
    @sdxRichEditCommandInsertPageBreakMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertPageCountFieldDescription',
    @sdxRichEditCommandInsertPageCountFieldDescription);
  AProduct.Add('sdxRichEditCommandInsertPageCountFieldMenuCaption',
    @sdxRichEditCommandInsertPageCountFieldMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertPageNumberFieldDescription',
    @sdxRichEditCommandInsertPageNumberFieldDescription);
  AProduct.Add('sdxRichEditCommandInsertPageNumberFieldMenuCaption',
    @sdxRichEditCommandInsertPageNumberFieldMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertPictureDescription',
    @sdxRichEditCommandInsertPictureDescription);
  AProduct.Add('sdxRichEditCommandInsertPictureMenuCaption',
    @sdxRichEditCommandInsertPictureMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertRegisteredTrademarkSymbolDescription',
    @sdxRichEditCommandInsertRegisteredTrademarkSymbolDescription);
  AProduct.Add('sdxRichEditCommandInsertRegisteredTrademarkSymbolMenuCaption',
    @sdxRichEditCommandInsertRegisteredTrademarkSymbolMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertSectionBreakContinuousDescription',
    @sdxRichEditCommandInsertSectionBreakContinuousDescription);
  AProduct.Add('sdxRichEditCommandInsertSectionBreakContinuousMenuCaption',
    @sdxRichEditCommandInsertSectionBreakContinuousMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertSectionBreakEvenPageDescription',
    @sdxRichEditCommandInsertSectionBreakEvenPageDescription);
  AProduct.Add('sdxRichEditCommandInsertSectionBreakEvenPageMenuCaption',
    @sdxRichEditCommandInsertSectionBreakEvenPageMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertSectionBreakNextPageDescription',
    @sdxRichEditCommandInsertSectionBreakNextPageDescription);
  AProduct.Add('sdxRichEditCommandInsertSectionBreakNextPageMenuCaption',
    @sdxRichEditCommandInsertSectionBreakNextPageMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertSectionBreakOddPageDescription',
    @sdxRichEditCommandInsertSectionBreakOddPageDescription);
  AProduct.Add('sdxRichEditCommandInsertSectionBreakOddPageMenuCaption',
    @sdxRichEditCommandInsertSectionBreakOddPageMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertSimpleListDescription',
    @sdxRichEditCommandInsertSimpleListDescription);
  AProduct.Add('sdxRichEditCommandInsertSimpleListMenuCaption',
    @sdxRichEditCommandInsertSimpleListMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertSymbolDescription',
    @sdxRichEditCommandInsertSymbolDescription);
  AProduct.Add('sdxRichEditCommandInsertSymbolMenuCaption',
    @sdxRichEditCommandInsertSymbolMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTabDescription',
    @sdxRichEditCommandInsertTabDescription);
  AProduct.Add('sdxRichEditCommandInsertTabMenuCaption',
    @sdxRichEditCommandInsertTabMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTextDescription',
    @sdxRichEditCommandInsertTextDescription);
  AProduct.Add('sdxRichEditCommandInsertTextMenuCaption',
    @sdxRichEditCommandInsertTextMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTrademarkSymbolDescription',
    @sdxRichEditCommandInsertTrademarkSymbolDescription);
  AProduct.Add('sdxRichEditCommandInsertTrademarkSymbolMenuCaption',
    @sdxRichEditCommandInsertTrademarkSymbolMenuCaption);
  AProduct.Add('sdxRichEditCommandLoadDocumentDescription',
    @sdxRichEditCommandLoadDocumentDescription);
  AProduct.Add('sdxRichEditCommandLoadDocumentMenuCaption',
    @sdxRichEditCommandLoadDocumentMenuCaption);
  AProduct.Add('sdxRichEditCommandMailMergeSaveDocumentAsCommandMenuCaption',
    @sdxRichEditCommandMailMergeSaveDocumentAsCommandMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveBackwardDescription',
    @sdxRichEditCommandMoveBackwardDescription);
  AProduct.Add('sdxRichEditCommandMoveBackwardMenuCaption',
    @sdxRichEditCommandMoveBackwardMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveForwardDescription',
    @sdxRichEditCommandMoveForwardDescription);
  AProduct.Add('sdxRichEditCommandMoveForwardMenuCaption',
    @sdxRichEditCommandMoveForwardMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveLineDownDescription',
    @sdxRichEditCommandMoveLineDownDescription);
  AProduct.Add('sdxRichEditCommandMoveLineDownMenuCaption',
    @sdxRichEditCommandMoveLineDownMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveLineUpDescription',
    @sdxRichEditCommandMoveLineUpDescription);
  AProduct.Add('sdxRichEditCommandMoveLineUpMenuCaption',
    @sdxRichEditCommandMoveLineUpMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveNextPageDescription',
    @sdxRichEditCommandMoveNextPageDescription);
  AProduct.Add('sdxRichEditCommandMoveNextPageMenuCaption',
    @sdxRichEditCommandMoveNextPageMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveNextWordDescription',
    @sdxRichEditCommandMoveNextWordDescription);
  AProduct.Add('sdxRichEditCommandMoveNextWordMenuCaption',
    @sdxRichEditCommandMoveNextWordMenuCaption);
  AProduct.Add('sdxRichEditCommandMovePreviousPageDescription',
    @sdxRichEditCommandMovePreviousPageDescription);
  AProduct.Add('sdxRichEditCommandMovePreviousPageMenuCaption',
    @sdxRichEditCommandMovePreviousPageMenuCaption);
  AProduct.Add('sdxRichEditCommandMovePreviousWordDescription',
    @sdxRichEditCommandMovePreviousWordDescription);
  AProduct.Add('sdxRichEditCommandMovePreviousWordMenuCaption',
    @sdxRichEditCommandMovePreviousWordMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveScreenDownDescription',
    @sdxRichEditCommandMoveScreenDownDescription);
  AProduct.Add('sdxRichEditCommandMoveScreenDownMenuCaption',
    @sdxRichEditCommandMoveScreenDownMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveScreenUpDescription',
    @sdxRichEditCommandMoveScreenUpDescription);
  AProduct.Add('sdxRichEditCommandMoveScreenUpMenuCaption',
    @sdxRichEditCommandMoveScreenUpMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveToBeginOfDocumentDescription',
    @sdxRichEditCommandMoveToBeginOfDocumentDescription);
  AProduct.Add('sdxRichEditCommandMoveToBeginOfDocumentMenuCaption',
    @sdxRichEditCommandMoveToBeginOfDocumentMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveToEndOfDocumentDescription',
    @sdxRichEditCommandMoveToEndOfDocumentDescription);
  AProduct.Add('sdxRichEditCommandMoveToEndOfDocumentMenuCaption',
    @sdxRichEditCommandMoveToEndOfDocumentMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveToEndOfLineDescription',
    @sdxRichEditCommandMoveToEndOfLineDescription);
  AProduct.Add('sdxRichEditCommandMoveToEndOfLineMenuCaption',
    @sdxRichEditCommandMoveToEndOfLineMenuCaption);
  AProduct.Add('sdxRichEditCommandMoveToStartOfLineDescription',
    @sdxRichEditCommandMoveToStartOfLineDescription);
  AProduct.Add('sdxRichEditCommandMoveToStartOfLineMenuCaption',
    @sdxRichEditCommandMoveToStartOfLineMenuCaption);
  AProduct.Add('sdxRichEditCommandNewEmptyDocumentDescription',
    @sdxRichEditCommandNewEmptyDocumentDescription);
  AProduct.Add('sdxRichEditCommandNewEmptyDocumentMenuCaption',
    @sdxRichEditCommandNewEmptyDocumentMenuCaption);
  AProduct.Add('sdxRichEditCommandOpenHyperlinkAtCaretPositionDescription',
    @sdxRichEditCommandOpenHyperlinkAtCaretPositionDescription);
  AProduct.Add('sdxRichEditCommandOpenHyperlinkAtCaretPositionMenuCaption',
    @sdxRichEditCommandOpenHyperlinkAtCaretPositionMenuCaption);
  AProduct.Add('sdxRichEditCommandOpenHyperlinkDescription',
    @sdxRichEditCommandOpenHyperlinkDescription);
  AProduct.Add('sdxRichEditCommandOpenHyperlinkMenuCaption',
    @sdxRichEditCommandOpenHyperlinkMenuCaption);
  AProduct.Add('sdxRichEditCommandOvertypeTextDescription',
    @sdxRichEditCommandOvertypeTextDescription);
  AProduct.Add('sdxRichEditCommandOvertypeTextMenuCaption',
    @sdxRichEditCommandOvertypeTextMenuCaption);
  AProduct.Add('sdxRichEditCommandPasteDescription',
    @sdxRichEditCommandPasteDescription);
  AProduct.Add('sdxRichEditCommandPasteFilesDescription',
    @sdxRichEditCommandPasteFilesDescription);
  AProduct.Add('sdxRichEditCommandPasteFilesMenuCaption',
    @sdxRichEditCommandPasteFilesMenuCaption);
  AProduct.Add('sdxRichEditCommandPasteHtmlTextDescription',
    @sdxRichEditCommandPasteHtmlTextDescription);
  AProduct.Add('sdxRichEditCommandPasteHtmlTextMenuCaption',
    @sdxRichEditCommandPasteHtmlTextMenuCaption);
  AProduct.Add('sdxRichEditCommandPasteImageDescription',
    @sdxRichEditCommandPasteImageDescription);
  AProduct.Add('sdxRichEditCommandPasteImageMenuCaption',
    @sdxRichEditCommandPasteImageMenuCaption);
  AProduct.Add('sdxRichEditCommandPasteMenuCaption',
    @sdxRichEditCommandPasteMenuCaption);
  AProduct.Add('sdxRichEditCommandPasteMetafileImageDescription',
    @sdxRichEditCommandPasteMetafileImageDescription);
  AProduct.Add('sdxRichEditCommandPasteMetafileImageMenuCaption',
    @sdxRichEditCommandPasteMetafileImageMenuCaption);
  AProduct.Add('sdxRichEditCommandPastePlainTextDescription',
    @sdxRichEditCommandPastePlainTextDescription);
  AProduct.Add('sdxRichEditCommandPastePlainTextMenuCaption',
    @sdxRichEditCommandPastePlainTextMenuCaption);
  AProduct.Add('sdxRichEditCommandPasteRtfTextDescription',
    @sdxRichEditCommandPasteRtfTextDescription);
  AProduct.Add('sdxRichEditCommandPasteRtfTextMenuCaption',
    @sdxRichEditCommandPasteRtfTextMenuCaption);
  AProduct.Add('sdxRichEditCommandPlaceCaretToPhysicalPointDescription',
    @sdxRichEditCommandPlaceCaretToPhysicalPointDescription);
  AProduct.Add('sdxRichEditCommandPlaceCaretToPhysicalPointMenuCaption',
    @sdxRichEditCommandPlaceCaretToPhysicalPointMenuCaption);
  AProduct.Add('sdxRichEditCommandPrintDescription',
    @sdxRichEditCommandPrintDescription);
  AProduct.Add('sdxRichEditCommandPrintMenuCaption',
    @sdxRichEditCommandPrintMenuCaption);
  AProduct.Add('sdxRichEditCommandPrintPreviewDescription',
    @sdxRichEditCommandPrintPreviewDescription);
  AProduct.Add('sdxRichEditCommandPrintPreviewMenuCaption',
    @sdxRichEditCommandPrintPreviewMenuCaption);
  AProduct.Add('sdxRichEditCommandRedoDescription',
    @sdxRichEditCommandRedoDescription);
  AProduct.Add('sdxRichEditCommandRedoMenuCaption',
    @sdxRichEditCommandRedoMenuCaption);
  AProduct.Add('sdxRichEditCommandRemoveHyperlinkDescription',
    @sdxRichEditCommandRemoveHyperlinkDescription);
  AProduct.Add('sdxRichEditCommandRemoveHyperlinkMenuCaption',
    @sdxRichEditCommandRemoveHyperlinkMenuCaption);
  AProduct.Add('sdxRichEditCommandReplaceAllBackwardDescription',
    @sdxRichEditCommandReplaceAllBackwardDescription);
  AProduct.Add('sdxRichEditCommandReplaceAllBackwardMenuCaption',
    @sdxRichEditCommandReplaceAllBackwardMenuCaption);
  AProduct.Add('sdxRichEditCommandReplaceAllForwardDescription',
    @sdxRichEditCommandReplaceAllForwardDescription);
  AProduct.Add('sdxRichEditCommandReplaceBackwardDescription',
    @sdxRichEditCommandReplaceBackwardDescription);
  AProduct.Add('sdxRichEditCommandReplaceBackwardMenuCaption',
    @sdxRichEditCommandReplaceBackwardMenuCaption);
  AProduct.Add('sdxRichEditCommandReplaceDescription',
    @sdxRichEditCommandReplaceDescription);
  AProduct.Add('sdxRichEditCommandReplaceForwardDescription',
    @sdxRichEditCommandReplaceForwardDescription);
  AProduct.Add('sdxRichEditCommandReplaceForwardMenuCaption',
    @sdxRichEditCommandReplaceForwardMenuCaption);
  AProduct.Add('sdxRichEditCommandReplaceMenuCaption',
    @sdxRichEditCommandReplaceMenuCaption);
  AProduct.Add('sdxRichEditCommandSaveDocumentAsDescription',
    @sdxRichEditCommandSaveDocumentAsDescription);
  AProduct.Add('sdxRichEditCommandSaveDocumentAsMenuCaption',
    @sdxRichEditCommandSaveDocumentAsMenuCaption);
  AProduct.Add('sdxRichEditCommandSaveDocumentDescription',
    @sdxRichEditCommandSaveDocumentDescription);
  AProduct.Add('sdxRichEditCommandSaveDocumentMenuCaption',
    @sdxRichEditCommandSaveDocumentMenuCaption);
  AProduct.Add('sdxRichEditCommandSelectAllDescription',
    @sdxRichEditCommandSelectAllDescription);
  AProduct.Add('sdxRichEditCommandSelectAllMenuCaption',
    @sdxRichEditCommandSelectAllMenuCaption);
  AProduct.Add('sdxRichEditCommandSelectBookmarkDescription',
    @sdxRichEditCommandSelectBookmarkDescription);
  AProduct.Add('sdxRichEditCommandSelectBookmarkMenuCaption',
    @sdxRichEditCommandSelectBookmarkMenuCaption);
  AProduct.Add('sdxRichEditCommandSelectFieldNextToCaretDescription',
    @sdxRichEditCommandSelectFieldNextToCaretDescription);
  AProduct.Add('sdxRichEditCommandSelectFieldNextToCaretMenuCaption',
    @sdxRichEditCommandSelectFieldNextToCaretMenuCaption);
  AProduct.Add('sdxRichEditCommandSelectFieldPrevToCaretDescription',
    @sdxRichEditCommandSelectFieldPrevToCaretDescription);
  AProduct.Add('sdxRichEditCommandSelectFieldPrevToCaretMenuCaption',
    @sdxRichEditCommandSelectFieldPrevToCaretMenuCaption);
  AProduct.Add('sdxRichEditCommandSetLandscapePageOrientationDescription',
    @sdxRichEditCommandSetLandscapePageOrientationDescription);
  AProduct.Add('sdxRichEditCommandSetLandscapePageOrientationMenuCaption',
    @sdxRichEditCommandSetLandscapePageOrientationMenuCaption);
  AProduct.Add('sdxRichEditCommandSetPortraitPageOrientationDescription',
    @sdxRichEditCommandSetPortraitPageOrientationDescription);
  AProduct.Add('sdxRichEditCommandSetPortraitPageOrientationMenuCaption',
    @sdxRichEditCommandSetPortraitPageOrientationMenuCaption);
  AProduct.Add('sdxRichEditCommandSetSectionColumnsDescription',
    @sdxRichEditCommandSetSectionColumnsDescription);
  AProduct.Add('sdxRichEditCommandSetSectionOneColumnDescription',
    @sdxRichEditCommandSetSectionOneColumnDescription);
  AProduct.Add('sdxRichEditCommandSetSectionOneColumnMenuCaption',
    @sdxRichEditCommandSetSectionOneColumnMenuCaption);
  AProduct.Add('sdxRichEditCommandSetSectionThreeColumnsDescription',
    @sdxRichEditCommandSetSectionThreeColumnsDescription);
  AProduct.Add('sdxRichEditCommandSetSectionThreeColumnsMenuCaption',
    @sdxRichEditCommandSetSectionThreeColumnsMenuCaption);
  AProduct.Add('sdxRichEditCommandSetSectionTwoColumnsDescription',
    @sdxRichEditCommandSetSectionTwoColumnsDescription);
  AProduct.Add('sdxRichEditCommandSetSectionTwoColumnsMenuCaption',
    @sdxRichEditCommandSetSectionTwoColumnsMenuCaption);
  AProduct.Add('sdxRichEditCommandShowAllFieldCodesDescription',
    @sdxRichEditCommandShowAllFieldCodesDescription);
  AProduct.Add('sdxRichEditCommandShowAllFieldCodesMenuCaption',
    @sdxRichEditCommandShowAllFieldCodesMenuCaption);
  AProduct.Add('sdxRichEditCommandShowAllFieldResultsDescription',
    @sdxRichEditCommandShowAllFieldResultsDescription);
  AProduct.Add('sdxRichEditCommandShowAllFieldResultsMenuCaption',
    @sdxRichEditCommandShowAllFieldResultsMenuCaption);
  AProduct.Add('sdxRichEditCommandShowEditStyleFormDescription',
    @sdxRichEditCommandShowEditStyleFormDescription);
  AProduct.Add('sdxRichEditCommandShowEditStyleFormMenuCaption',
    @sdxRichEditCommandShowEditStyleFormMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandShowFloatingObjectLayoutOptionsFormDescription',
    @sdxRichEditCommandShowFloatingObjectLayoutOptionsFormDescription);
  AProduct.Add
    ('sdxRichEditCommandShowFloatingObjectLayoutOptionsFormMenuCaption',
    @sdxRichEditCommandShowFloatingObjectLayoutOptionsFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowFontFormDescription',
    @sdxRichEditCommandShowFontFormDescription);
  AProduct.Add('sdxRichEditCommandShowFontFormMenuCaption',
    @sdxRichEditCommandShowFontFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowHyperlinkFormDescription',
    @sdxRichEditCommandShowHyperlinkFormDescription);
  AProduct.Add('sdxRichEditCommandShowHyperlinkFormMenuCaption',
    @sdxRichEditCommandShowHyperlinkFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowInsertMergeFieldFormDescription',
    @sdxRichEditCommandShowInsertMergeFieldFormDescription);
  AProduct.Add('sdxRichEditCommandShowInsertMergeFieldFormMenuCaption',
    @sdxRichEditCommandShowInsertMergeFieldFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowNumberingListDescription',
    @sdxRichEditCommandShowNumberingListDescription);
  AProduct.Add('sdxRichEditCommandShowNumberingListMenuCaption',
    @sdxRichEditCommandShowNumberingListMenuCaption);
  AProduct.Add('sdxRichEditCommandShowRangeEditingPermissionsFormDescription',
    @sdxRichEditCommandShowRangeEditingPermissionsFormDescription);
  AProduct.Add('sdxRichEditCommandShowRangeEditingPermissionsFormMenuCaption',
    @sdxRichEditCommandShowRangeEditingPermissionsFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowSpellingDialogMenuCaption',
    @sdxRichEditCommandShowSpellingDialogMenuCaption);
  AProduct.Add('sdxRichEditCommandShowSpellingDialogDescription',
    @sdxRichEditCommandShowSpellingDialogDescription);
  AProduct.Add('sdxRichEditCommandShowSymbolDescription',
    @sdxRichEditCommandShowSymbolDescription);
  AProduct.Add('sdxRichEditCommandShowSymbolMenuCaption',
    @sdxRichEditCommandShowSymbolMenuCaption);
  AProduct.Add('sdxRichEditCommandShowTabsFormDescription',
    @sdxRichEditCommandShowTabsFormDescription);
  AProduct.Add('sdxRichEditCommandShowTabsFormMenuCaption',
    @sdxRichEditCommandShowTabsFormMenuCaption);
  AProduct.Add('sdxRichEditCommandSwitchToDraftViewDescription',
    @sdxRichEditCommandSwitchToDraftViewDescription);
  AProduct.Add('sdxRichEditCommandSwitchToDraftViewMenuCaption',
    @sdxRichEditCommandSwitchToDraftViewMenuCaption);
  AProduct.Add('sdxRichEditCommandSwitchToPrintLayoutViewDescription',
    @sdxRichEditCommandSwitchToPrintLayoutViewDescription);
  AProduct.Add('sdxRichEditCommandSwitchToPrintLayoutViewMenuCaption',
    @sdxRichEditCommandSwitchToPrintLayoutViewMenuCaption);
  AProduct.Add('sdxRichEditCommandSwitchToSimpleViewDescription',
    @sdxRichEditCommandSwitchToSimpleViewDescription);
  AProduct.Add('sdxRichEditCommandSwitchToSimpleViewMenuCaption',
    @sdxRichEditCommandSwitchToSimpleViewMenuCaption);
  AProduct.Add('sdxRichEditCommandTabKeyDescription',
    @sdxRichEditCommandTabKeyDescription);
  AProduct.Add('sdxRichEditCommandTabKeyMenuCaption',
    @sdxRichEditCommandTabKeyMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleDifferentFirstPageDescription',
    @sdxRichEditCommandToggleDifferentFirstPageDescription);
  AProduct.Add('sdxRichEditCommandToggleDifferentFirstPageMenuCaption',
    @sdxRichEditCommandToggleDifferentFirstPageMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleDifferentOddAndEvenPagesDescription',
    @sdxRichEditCommandToggleDifferentOddAndEvenPagesDescription);
  AProduct.Add('sdxRichEditCommandToggleDifferentOddAndEvenPagesMenuCaption',
    @sdxRichEditCommandToggleDifferentOddAndEvenPagesMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleFieldCodesDescription',
    @sdxRichEditCommandToggleFieldCodesDescription);
  AProduct.Add('sdxRichEditCommandToggleFieldCodesMenuCaption',
    @sdxRichEditCommandToggleFieldCodesMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleFontBoldDescription',
    @sdxRichEditCommandToggleFontBoldDescription);
  AProduct.Add('sdxRichEditCommandToggleFontBoldMenuCaption',
    @sdxRichEditCommandToggleFontBoldMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleFontDoubleStrikeoutDescription',
    @sdxRichEditCommandToggleFontDoubleStrikeoutDescription);
  AProduct.Add('sdxRichEditCommandToggleFontDoubleStrikeoutMenuCaption',
    @sdxRichEditCommandToggleFontDoubleStrikeoutMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleFontDoubleUnderlineDescription',
    @sdxRichEditCommandToggleFontDoubleUnderlineDescription);
  AProduct.Add('sdxRichEditCommandToggleFontDoubleUnderlineMenuCaption',
    @sdxRichEditCommandToggleFontDoubleUnderlineMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleFontItalicDescription',
    @sdxRichEditCommandToggleFontItalicDescription);
  AProduct.Add('sdxRichEditCommandToggleFontItalicMenuCaption',
    @sdxRichEditCommandToggleFontItalicMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleFontStrikeoutDescription',
    @sdxRichEditCommandToggleFontStrikeoutDescription);
  AProduct.Add('sdxRichEditCommandToggleFontStrikeoutMenuCaption',
    @sdxRichEditCommandToggleFontStrikeoutMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleFontUnderlineDescription',
    @sdxRichEditCommandToggleFontUnderlineDescription);
  AProduct.Add('sdxRichEditCommandToggleFontUnderlineMenuCaption',
    @sdxRichEditCommandToggleFontUnderlineMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleHeaderFooterLinkToPreviousDescription',
    @sdxRichEditCommandToggleHeaderFooterLinkToPreviousDescription);
  AProduct.Add('sdxRichEditCommandToggleHeaderFooterLinkToPreviousMenuCaption',
    @sdxRichEditCommandToggleHeaderFooterLinkToPreviousMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleOvertypeDescription',
    @sdxRichEditCommandToggleOvertypeDescription);
  AProduct.Add('sdxRichEditCommandToggleOvertypeMenuCaption',
    @sdxRichEditCommandToggleOvertypeMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleShowHorizontalRulerDescription',
    @sdxRichEditCommandToggleShowHorizontalRulerDescription);
  AProduct.Add('sdxRichEditCommandToggleShowHorizontalRulerMenuCaption',
    @sdxRichEditCommandToggleShowHorizontalRulerMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleShowVerticalRulerDescription',
    @sdxRichEditCommandToggleShowVerticalRulerDescription);
  AProduct.Add('sdxRichEditCommandToggleShowVerticalRulerMenuCaption',
    @sdxRichEditCommandToggleShowVerticalRulerMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleViewMergedDataDescription',
    @sdxRichEditCommandToggleViewMergedDataDescription);
  AProduct.Add('sdxRichEditCommandToggleViewMergedDataMenuCaption',
    @sdxRichEditCommandToggleViewMergedDataMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleWhitespaceDescription',
    @sdxRichEditCommandToggleWhitespaceDescription);
  AProduct.Add('sdxRichEditCommandToggleWhitespaceMenuCaption',
    @sdxRichEditCommandToggleWhitespaceMenuCaption);
  AProduct.Add('sdxRichEditCommandUndoDescription',
    @sdxRichEditCommandUndoDescription);
  AProduct.Add('sdxRichEditCommandUndoMenuCaption',
    @sdxRichEditCommandUndoMenuCaption);
  AProduct.Add('sdxRichEditCommandUpdateFieldDescription',
    @sdxRichEditCommandUpdateFieldDescription);
  AProduct.Add('sdxRichEditCommandUpdateFieldMenuCaption',
    @sdxRichEditCommandUpdateFieldMenuCaption);
  AProduct.Add('sdxRichEditCommandUpdateFieldsDescription',
    @sdxRichEditCommandUpdateFieldsDescription);
  AProduct.Add('sdxRichEditCommandUpdateFieldsMenuCaption',
    @sdxRichEditCommandUpdateFieldsMenuCaption);
  AProduct.Add('sdxRichEditCommandZoomDescription',
    @sdxRichEditCommandZoomDescription);
  AProduct.Add('sdxRichEditCommandZoomInDescription',
    @sdxRichEditCommandZoomInDescription);
  AProduct.Add('sdxRichEditCommandZoomInMenuCaption',
    @sdxRichEditCommandZoomInMenuCaption);
  AProduct.Add('sdxRichEditCommandZoomMenuCaption',
    @sdxRichEditCommandZoomMenuCaption);
  AProduct.Add('sdxRichEditCommandZoomOutDescription',
    @sdxRichEditCommandZoomOutDescription);
  AProduct.Add('sdxRichEditCommandZoomOutMenuCaption',
    @sdxRichEditCommandZoomOutMenuCaption);

  AProduct.Add('sdxRichEditCommandProtectDocumentMenuCaption',
    @sdxRichEditCommandProtectDocumentMenuCaption);
  AProduct.Add('sdxRichEditCommandProtectDocumentDescription',
    @sdxRichEditCommandProtectDocumentDescription);
  AProduct.Add('sdxRichEditCommandUnprotectDocumentMenuCaption',
    @sdxRichEditCommandUnprotectDocumentMenuCaption);
  AProduct.Add('sdxRichEditCommandUnprotectDocumentDescription',
    @sdxRichEditCommandUnprotectDocumentDescription);
  AProduct.Add('sdxRichEditCommandMakeTextUpperCaseMenuCaption',
    @sdxRichEditCommandMakeTextUpperCaseMenuCaption);
  AProduct.Add('sdxRichEditCommandMakeTextUpperCaseDescription',
    @sdxRichEditCommandMakeTextUpperCaseDescription);
  AProduct.Add('sdxRichEditCommandMakeTextLowerCaseMenuCaption',
    @sdxRichEditCommandMakeTextLowerCaseMenuCaption);
  AProduct.Add('sdxRichEditCommandMakeTextLowerCaseDescription',
    @sdxRichEditCommandMakeTextLowerCaseDescription);
  AProduct.Add('sdxRichEditCommandToggleTextCaseMenuCaption',
    @sdxRichEditCommandToggleTextCaseMenuCaption);
  AProduct.Add('sdxRichEditCommandToggleTextCaseDescription',
    @sdxRichEditCommandToggleTextCaseDescription);
  AProduct.Add('sdxRichEditCommandCapitalizeEachWordTextCaseMenuCaption',
    @sdxRichEditCommandCapitalizeEachWordTextCaseMenuCaption);
  AProduct.Add('sdxRichEditCommandCapitalizeEachWordTextCaseDescription',
    @sdxRichEditCommandCapitalizeEachWordTextCaseDescription);
  AProduct.Add('sdxRichEditCommandChangeTextCaseMenuCaption',
    @sdxRichEditCommandChangeTextCaseMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeTextCaseDescription',
    @sdxRichEditCommandChangeTextCaseDescription);
  AProduct.Add('sdxRichEditCommandChangeSectionLineNumberingMenuCaption',
    @sdxRichEditCommandChangeSectionLineNumberingMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeSectionLineNumberingDescription',
    @sdxRichEditCommandChangeSectionLineNumberingDescription);
  AProduct.Add('sdxRichEditCommandSetSectionLineNumberingNoneMenuCaption',
    @sdxRichEditCommandSetSectionLineNumberingNoneMenuCaption);
  AProduct.Add('sdxRichEditCommandSetSectionLineNumberingNoneDescription',
    @sdxRichEditCommandSetSectionLineNumberingNoneDescription);
  AProduct.Add('sdxRichEditCommandSetSectionLineNumberingContinuousMenuCaption',
    @sdxRichEditCommandSetSectionLineNumberingContinuousMenuCaption);
  AProduct.Add('sdxRichEditCommandSetSectionLineNumberingContinuousDescription',
    @sdxRichEditCommandSetSectionLineNumberingContinuousDescription);
  AProduct.Add
    ('sdxRichEditCommandSetSectionLineNumberingRestartNewPageMenuCaption',
    @sdxRichEditCommandSetSectionLineNumberingRestartNewPageMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetSectionLineNumberingRestartNewPageDescription',
    @sdxRichEditCommandSetSectionLineNumberingRestartNewPageDescription);
  AProduct.Add
    ('sdxRichEditCommandSetSectionLineNumberingRestartNewSectionMenuCaption',
    @sdxRichEditCommandSetSectionLineNumberingRestartNewSectionMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetSectionLineNumberingRestartNewSectionDescription',
    @sdxRichEditCommandSetSectionLineNumberingRestartNewSectionDescription);
  AProduct.Add('sdxRichEditCommandShowLineNumberingFormMenuCaption',
    @sdxRichEditCommandShowLineNumberingFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowLineNumberingFormDescription',
    @sdxRichEditCommandShowLineNumberingFormDescription);
  AProduct.Add('sdxRichEditCommandShowPageSetupFormMenuCaption',
    @sdxRichEditCommandShowPageSetupFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowPageSetupFormDescription',
    @sdxRichEditCommandShowPageSetupFormDescription);
  AProduct.Add('sdxRichEditCommandShowColumnsSetupFormMenuCaption',
    @sdxRichEditCommandShowColumnsSetupFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowColumnsSetupFormDescription',
    @sdxRichEditCommandShowColumnsSetupFormDescription);
  AProduct.Add('sdxRichEditCommandInsertEquationCaptionMenuCaption',
    @sdxRichEditCommandInsertEquationCaptionMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertEquationCaptionDescription',
    @sdxRichEditCommandInsertEquationCaptionDescription);
  AProduct.Add('sdxRichEditCommandInsertFigureCaptionMenuCaption',
    @sdxRichEditCommandInsertFigureCaptionMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertFigureCaptionDescription',
    @sdxRichEditCommandInsertFigureCaptionDescription);
  AProduct.Add('sdxRichEditCommandInsertCaptionPlaceholderMenuCaption',
    @sdxRichEditCommandInsertCaptionPlaceholderMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertCaptionPlaceholderDescription',
    @sdxRichEditCommandInsertCaptionPlaceholderDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectSquareTextWrapTypeMenuCaption',
    @sdxRichEditCommandSetFloatingObjectSquareTextWrapTypeMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectSquareTextWrapTypeDescription',
    @sdxRichEditCommandSetFloatingObjectSquareTextWrapTypeDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectBehindTextWrapTypeMenuCaption',
    @sdxRichEditCommandSetFloatingObjectBehindTextWrapTypeMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectBehindTextWrapTypeDescription',
    @sdxRichEditCommandSetFloatingObjectBehindTextWrapTypeDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectInFrontOfTextWrapTypeMenuCaption',
    @sdxRichEditCommandSetFloatingObjectInFrontOfTextWrapTypeMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectInFrontOfTextWrapTypeDescription',
    @sdxRichEditCommandSetFloatingObjectInFrontOfTextWrapTypeDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectThroughTextWrapTypeMenuCaption',
    @sdxRichEditCommandSetFloatingObjectThroughTextWrapTypeMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectThroughTextWrapTypeDescription',
    @sdxRichEditCommandSetFloatingObjectThroughTextWrapTypeDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectTightTextWrapTypeMenuCaption',
    @sdxRichEditCommandSetFloatingObjectTightTextWrapTypeMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectTightTextWrapTypeDescription',
    @sdxRichEditCommandSetFloatingObjectTightTextWrapTypeDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectTopAndBottomTextWrapTypeMenuCaption',
    @sdxRichEditCommandSetFloatingObjectTopAndBottomTextWrapTypeMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectTopAndBottomTextWrapTypeDescription',
    @sdxRichEditCommandSetFloatingObjectTopAndBottomTextWrapTypeDescription);
  AProduct.Add('sdxRichEditCommandFloatingObjectBringForwardMenuCaption',
    @sdxRichEditCommandFloatingObjectBringForwardMenuCaption);
  AProduct.Add('sdxRichEditCommandFloatingObjectBringForwardDescription',
    @sdxRichEditCommandFloatingObjectBringForwardDescription);
  AProduct.Add('sdxRichEditCommandFloatingObjectBringToFrontMenuCaption',
    @sdxRichEditCommandFloatingObjectBringToFrontMenuCaption);
  AProduct.Add('sdxRichEditCommandFloatingObjectBringToFrontDescription',
    @sdxRichEditCommandFloatingObjectBringToFrontDescription);
  AProduct.Add('sdxRichEditCommandFloatingObjectBringInFrontOfTextMenuCaption',
    @sdxRichEditCommandFloatingObjectBringInFrontOfTextMenuCaption);
  AProduct.Add('sdxRichEditCommandFloatingObjectBringInFrontOfTextDescription',
    @sdxRichEditCommandFloatingObjectBringInFrontOfTextDescription);
  AProduct.Add('sdxRichEditCommandFloatingObjectSendBackwardMenuCaption',
    @sdxRichEditCommandFloatingObjectSendBackwardMenuCaption);
  AProduct.Add('sdxRichEditCommandFloatingObjectSendBackwardDescription',
    @sdxRichEditCommandFloatingObjectSendBackwardDescription);
  AProduct.Add('sdxRichEditCommandFloatingObjectSendToBackMenuCaption',
    @sdxRichEditCommandFloatingObjectSendToBackMenuCaption);
  AProduct.Add('sdxRichEditCommandFloatingObjectSendToBackDescription',
    @sdxRichEditCommandFloatingObjectSendToBackDescription);
  AProduct.Add('sdxRichEditCommandFloatingObjectSendBehindTextMenuCaption',
    @sdxRichEditCommandFloatingObjectSendBehindTextMenuCaption);
  AProduct.Add('sdxRichEditCommandFloatingObjectSendBehindTextDescription',
    @sdxRichEditCommandFloatingObjectSendBehindTextDescription);
  AProduct.Add('sdxRichEditCommandSetFloatingObjectTopLeftAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectTopLeftAlignmentMenuCaption);
  AProduct.Add('sdxRichEditCommandSetFloatingObjectTopLeftAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectTopLeftAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectTopCenterAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectTopCenterAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectTopCenterAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectTopCenterAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectTopRightAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectTopRightAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectTopRightAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectTopRightAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectMiddleLeftAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectMiddleLeftAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectMiddleLeftAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectMiddleLeftAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectMiddleCenterAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectMiddleCenterAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectMiddleCenterAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectMiddleCenterAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectMiddleRightAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectMiddleRightAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectMiddleRightAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectMiddleRightAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectBottomLeftAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectBottomLeftAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectBottomLeftAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectBottomLeftAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectBottomCenterAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectBottomCenterAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectBottomCenterAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectBottomCenterAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectBottomRightAlignmentMenuCaption',
    @sdxRichEditCommandSetFloatingObjectBottomRightAlignmentMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandSetFloatingObjectBottomRightAlignmentDescription',
    @sdxRichEditCommandSetFloatingObjectBottomRightAlignmentDescription);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectTextWrapTypeMenuCaption',
    @sdxRichEditCommandChangeFloatingObjectTextWrapTypeMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectTextWrapTypeDescription',
    @sdxRichEditCommandChangeFloatingObjectTextWrapTypeDescription);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectAlignmentMenuCaption',
    @sdxRichEditCommandChangeFloatingObjectAlignmentMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectAlignmentDescription',
    @sdxRichEditCommandChangeFloatingObjectAlignmentDescription);
  AProduct.Add
    ('sdxRichEditCommandFloatingObjectBringForwardPlaceholderMenuCaption',
    @sdxRichEditCommandFloatingObjectBringForwardPlaceholderMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandFloatingObjectBringForwardPlaceholderDescription',
    @sdxRichEditCommandFloatingObjectBringForwardPlaceholderDescription);
  AProduct.Add
    ('sdxRichEditCommandFloatingObjectSendBackwardPlaceholderMenuCaption',
    @sdxRichEditCommandFloatingObjectSendBackwardPlaceholderMenuCaption);
  AProduct.Add
    ('sdxRichEditCommandFloatingObjectSendBackwardPlaceholderDescription',
    @sdxRichEditCommandFloatingObjectSendBackwardPlaceholderDescription);
  AProduct.Add('sdxRichEditCommandShowPageMarginsSetupFormMenuCaption',
    @sdxRichEditCommandShowPageMarginsSetupFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowPageMarginsSetupFormDescription',
    @sdxRichEditCommandShowPageMarginsSetupFormDescription);
  AProduct.Add('sdxRichEditCommandShowPagePaperSetupFormMenuCaption',
    @sdxRichEditCommandShowPagePaperSetupFormMenuCaption);
  AProduct.Add('sdxRichEditCommandShowPagePaperSetupFormDescription',
    @sdxRichEditCommandShowPagePaperSetupFormDescription);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectFillColorMenuCaption',
    @sdxRichEditCommandChangeFloatingObjectFillColorMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectFillColorDescription',
    @sdxRichEditCommandChangeFloatingObjectFillColorDescription);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectOutlineColorMenuCaption',
    @sdxRichEditCommandChangeFloatingObjectOutlineColorMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectOutlineColorDescription',
    @sdxRichEditCommandChangeFloatingObjectOutlineColorDescription);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectOutlineWidthMenuCaption',
    @sdxRichEditCommandChangeFloatingObjectOutlineWidthMenuCaption);
  AProduct.Add('sdxRichEditCommandChangeFloatingObjectOutlineWidthDescription',
    @sdxRichEditCommandChangeFloatingObjectOutlineWidthDescription);
  AProduct.Add('sdxRichEditCommandInsertTextBoxMenuCaption',
    @sdxRichEditCommandInsertTextBoxMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertTextBoxDescription',
    @sdxRichEditCommandInsertTextBoxDescription);
  AProduct.Add('sdxRichEditCommandInsertFloatingObjectPictureMenuCaption',
    @sdxRichEditCommandInsertFloatingObjectPictureMenuCaption);
  AProduct.Add('sdxRichEditCommandInsertFloatingObjectPictureDescription',
    @sdxRichEditCommandInsertFloatingObjectPictureDescription);
  AProduct.Add('sdxRichEditCommandChangePageColorMenuCaption',
    @sdxRichEditCommandChangePageColorMenuCaption);
  AProduct.Add('sdxRichEditCommandChangePageColorDescription',
    @sdxRichEditCommandChangePageColorDescription);

  AProduct.Add('sdxRichEditCommandCaptionPrefixTable',
    @sdxRichEditCommandCaptionPrefixTable);
  AProduct.Add('sdxRichEditCommandCaptionPrefixEquation',
    @sdxRichEditCommandCaptionPrefixEquation);
  AProduct.Add('sdxRichEditCommandCaptionPrefixFigure',
    @sdxRichEditCommandCaptionPrefixFigure);
  AProduct.Add('sdxRichEditInsertHyperlinkTitle',
    @sdxRichEditInsertHyperlinkTitle);
  AProduct.Add('sdxRichEditEditHyperlinkTitle', @sdxRichEditEditHyperlinkTitle);

  AProduct.Add('sdxRichEditCommandPaperSizeGalleryCaption',
    @sdxRichEditCommandPaperSizeGalleryCaption);
  AProduct.Add('sdxRichEditCommandPaperSizeGalleryUnitsInchesCaption',
    @sdxRichEditCommandPaperSizeGalleryUnitsInchesCaption);
  AProduct.Add('sdxRichEditCommandPaperSizeGalleryUnitsMillimetersCaption',
    @sdxRichEditCommandPaperSizeGalleryUnitsMillimetersCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryCaption',
    @sdxRichEditCommandPageMarginsGalleryCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryNormalMarginsCaption',
    @sdxRichEditCommandPageMarginsGalleryNormalMarginsCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryNarrowMarginsCaption',
    @sdxRichEditCommandPageMarginsGalleryNarrowMarginsCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryModerateMarginsCaption',
    @sdxRichEditCommandPageMarginsGalleryModerateMarginsCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryWideMarginsCaption',
    @sdxRichEditCommandPageMarginsGalleryWideMarginsCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryTopPartCaption',
    @sdxRichEditCommandPageMarginsGalleryTopPartCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryBottomPartCaption',
    @sdxRichEditCommandPageMarginsGalleryBottomPartCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryLeftPartCaption',
    @sdxRichEditCommandPageMarginsGalleryLeftPartCaption);
  AProduct.Add('sdxRichEditCommandPageMarginsGalleryRightPartCaption',
    @sdxRichEditCommandPageMarginsGalleryRightPartCaption);
  AProduct.Add('sdxRichEditCommandQuickStylesGalleryCaption',
    @sdxRichEditCommandQuickStylesGalleryCaption);
  AProduct.Add('sdxRichEditCommandTableStylesGalleryCaption',
    @sdxRichEditCommandTableStylesGalleryCaption);
end;

initialization

dxResourceStringsRepository.RegisterProduct(dxRichEditProductName,
  @AddRichEditCommandsStringNames);

finalization

dxResourceStringsRepository.UnRegisterProduct(dxRichEditProductName,
  @AddRichEditCommandsStringNames);

end.
