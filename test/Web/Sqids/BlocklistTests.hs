{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.BlocklistTests where

import Test.Hspec
import Web.Sqids.Internal

withEmptyBlocklist :: Sqids a -> Either SqidsError a
withEmptyBlocklist = runSqids defaultSqidsOptions{ blocklist = [] }

testBlocklist :: SpecWith ()
testBlocklist = do
  describe "blocklist" $ do
    it "if no custom blocklist param, use the default blocklist" $ do
      (sqids $ decode "sexy") `shouldBe` Right [ 200044 ]
      (sqids $ encode [ 200044 ]) `shouldBe` Right "d171vI"

    it "if an empty blocklist param passed, don't use any blocklist" $ do
      (withEmptyBlocklist $ decode "sexy") `shouldBe` Right [ 200044 ]
      (withEmptyBlocklist $ encode [ 200044 ]) `shouldBe` Right "sexy"

--test(`if an empty blocklist param passed, don't use any blocklist`, () => {
--	const sqids = new Sqids({
--		blocklist: new Set([])
--	});
--
--	expect.soft(sqids.decode('sexy')).toEqual([200044]);
--	expect.soft(sqids.encode([200044])).toBe('sexy');
--});
--
--test('if a non-empty blocklist param passed, use only that', () => {
--	const sqids = new Sqids({
--		blocklist: new Set([
--			'AvTg' // originally encoded [100000]
--		])
--	});
--
--	// make sure we don't use the default blocklist
--	expect.soft(sqids.decode('sexy')).toEqual([200044]);
--	expect.soft(sqids.encode([200044])).toBe('sexy');
--
--	// make sure we are using the passed blocklist
--	expect.soft(sqids.decode('AvTg')).toEqual([100000]);
--	expect.soft(sqids.encode([100000])).toBe('7T1X8k');
--	expect.soft(sqids.decode('7T1X8k')).toEqual([100000]);
--});
--
--test('blocklist', () => {
--	const sqids = new Sqids({
--		blocklist: new Set([
--			'8QRLaD', // normal result of 1st encoding, let's block that word on purpose
--			'7T1cd0dL', // result of 2nd encoding
--			'UeIe', // result of 3rd encoding is `RA8UeIe7`, let's block a substring
--			'imhw', // result of 4th encoding is `WM3Limhw`, let's block the postfix
--			'LfUQ' // result of 4th encoding is `LfUQh4HN`, let's block the prefix
--		])
--	});
--
--	expect.soft(sqids.encode([1, 2, 3])).toBe('TM0x1Mxz');
--	expect.soft(sqids.decode('TM0x1Mxz')).toEqual([1, 2, 3]);
--});
--
--test('decoding blocklist words should still work', () => {
--	const sqids = new Sqids({
--		blocklist: new Set(['8QRLaD', '7T1cd0dL', 'RA8UeIe7', 'WM3Limhw', 'LfUQh4HN'])
--	});
--
--	expect.soft(sqids.decode('8QRLaD')).toEqual([1, 2, 3]);
--	expect.soft(sqids.decode('7T1cd0dL')).toEqual([1, 2, 3]);
--	expect.soft(sqids.decode('RA8UeIe7')).toEqual([1, 2, 3]);
--	expect.soft(sqids.decode('WM3Limhw')).toEqual([1, 2, 3]);
--	expect.soft(sqids.decode('LfUQh4HN')).toEqual([1, 2, 3]);
--});
--
--test('match against a short blocklist word', () => {
--	const sqids = new Sqids({
--		blocklist: new Set(['pPQ'])
--	});
--
--	expect.soft(sqids.decode(sqids.encode([1000]))).toEqual([1000]);
--});

